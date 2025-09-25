{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Starter.Auth.Session
  ( SessionConfig (..),
    Protected,
    SessionUser (..),
    loadSessionConfigFromEnv,
    mkSessionCookie,
    sessionContext
  )
where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import qualified Control.Monad.Except as Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor (first)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, decodeUtf8', encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, secondsToDiffTime)
import Network.HTTP.Types (hCookie)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wai (Request, rawPathInfo, rawQueryString, requestHeaders, requestMethod)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Starter.Auth.Firebase (FirebaseUser (..))
import Starter.Prelude
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Cookie
  ( SetCookie,
    defaultSetCookie,
    parseCookies,
    sameSiteLax,
    setCookieExpires,
    setCookieHttpOnly,
    setCookieMaxAge,
    setCookieName,
    setCookiePath,
    setCookieSameSite,
    setCookieSecure,
    setCookieValue
  )

-- | Session configuration required to mint and verify cookies.
data SessionConfig = SessionConfig
  { sessionSecret :: !ByteString,
    sessionCookieName :: !ByteString,
    sessionCookieMaxAge :: !NominalDiffTime,
    sessionLoginPath :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | Protected routes guard.
type Protected api = AuthProtect "session" :> api

newtype SessionUser = SessionUser {unSessionUser :: FirebaseUser}
  deriving stock (Eq, Show)

type instance AuthServerData (AuthProtect "session") = SessionUser

-- | Load session settings from environment variables.
loadSessionConfigFromEnv :: IO (Either Text SessionConfig)
loadSessionConfigFromEnv = do
  secretEnv <- lookupEnv "SESSION_SECRET"
  case secretEnv of
    Nothing -> pure (Left "SESSION_SECRET is not set")
    Just rawSecret -> do
      let secret = encodeUtf8 (Text.pack rawSecret)
      if BS.null secret
        then pure (Left "SESSION_SECRET cannot be empty")
        else do
          cookieNameEnv <- lookupEnv "SESSION_COOKIE_NAME"
          let cookieName =
                case fmap (encodeUtf8 . Text.pack) cookieNameEnv of
                  Just name | not (BS.null name) -> name
                  _ -> "hs_session"
          maxAgeEnv <- lookupEnv "SESSION_COOKIE_MAX_AGE_SECONDS"
          maxAgeSeconds <- case maxAgeEnv of
            Nothing -> pure defaultSessionMaxAge
            Just raw ->
              case readMaybe raw :: Maybe Integer of
                Nothing -> pure defaultSessionMaxAge
                Just seconds | seconds <= 0 -> pure defaultSessionMaxAge
                Just seconds -> pure seconds
          loginPathEnv <- lookupEnv "SESSION_LOGIN_PATH"
          let loginPath = maybe "/login" Text.pack loginPathEnv
          pure
            ( Right
                SessionConfig
                  { sessionSecret = secret,
                    sessionCookieName = cookieName,
                    sessionCookieMaxAge = fromInteger maxAgeSeconds,
                    sessionLoginPath = loginPath
                  }
            )
  where
    defaultSessionMaxAge :: Integer
    defaultSessionMaxAge = 60 * 60 * 24 * 7 -- one week

-- | Produce a 'Set-Cookie' header for the provided Firebase user.
mkSessionCookie :: SessionConfig -> UTCTime -> FirebaseUser -> SetCookie
mkSessionCookie SessionConfig {sessionSecret, sessionCookieName, sessionCookieMaxAge} now user =
  let payloadRaw = BL.toStrict (Aeson.encode user)
      payload = B64.encodeBase64' payloadRaw
      mac = hmac sessionSecret payload :: HMAC SHA256
      signature = B64.encodeBase64' (BA.convert mac :: ByteString)
      token = payload <> "." <> signature
      expiresAt = addUTCTime sessionCookieMaxAge now
   in defaultSetCookie
        { setCookieName = sessionCookieName,
          setCookieValue = token,
          setCookieHttpOnly = True,
          setCookieSecure = True,
          setCookieSameSite = Just sameSiteLax,
          setCookiePath = Just "/",
          setCookieExpires = Just expiresAt,
          setCookieMaxAge = Just (secondsToDiffTime (round sessionCookieMaxAge))
        }

-- | Build the Servant context needed for 'serveWithContext'.
sessionContext :: SessionConfig -> Context (AuthHandler Request SessionUser ': '[])
sessionContext config = sessionAuthHandler config :. EmptyContext

sessionAuthHandler :: SessionConfig -> AuthHandler Request SessionUser
sessionAuthHandler config =
  mkAuthHandler $ \req -> do
    let cookies = maybe [] parseCookies (lookup hCookie (requestHeaders req))
    case lookup (sessionCookieName config) cookies of
      Nothing -> Except.throwError (redirectToLogin config req "Missing session cookie")
      Just token ->
        case decodeSessionCookie config token of
          Left err -> Except.throwError err
          Right user -> pure (SessionUser user)

-- | Parse and verify a cookie.
decodeSessionCookie :: SessionConfig -> ByteString -> Either ServerError FirebaseUser
decodeSessionCookie SessionConfig {sessionSecret, sessionLoginPath} raw =
  case BS.break (== 46) raw of -- '.'
    (payloadB64, rest)
      | BS.null payloadB64 || BS.null rest -> Left invalidSession
      | otherwise ->
          let signatureB64 = BS.tail rest
           in do
                payload <- first (const invalidSession) (B64.decodeBase64 payloadB64)
                signature <- first (const invalidSession) (B64.decodeBase64 signatureB64)
                let mac = hmac sessionSecret payloadB64 :: HMAC SHA256
                    expected = BA.convert mac :: ByteString
                if BA.constEq expected signature
                  then case Aeson.eitherDecodeStrict payload of
                    Left _ -> Left invalidSession
                    Right user -> Right user
                  else Left invalidSession
  where
    invalidSession = redirectWithMessage "Invalid session. Please log in again."
    redirectWithMessage msg =
      let err = err303 {errBody = textBody msg}
       in attachRedirect sessionLoginPath Nothing err

redirectToLogin :: SessionConfig -> Request -> Text -> ServerError
redirectToLogin SessionConfig {sessionLoginPath} req msg =
  attachRedirect sessionLoginPath (sanitizeReturnTo req) err
  where
    err = err303 {errBody = textBody msg}

sanitizeReturnTo :: Request -> Maybe Text
sanitizeReturnTo req
  | requestMethod req /= "GET" = Nothing
  | otherwise =
      case decodeUtf8' raw of
        Left _ -> Nothing
        Right txt ->
          if Text.isPrefixOf "/" txt && not (Text.isPrefixOf "//" txt)
            then Just txt
            else Nothing
  where
    raw = rawPathInfo req <> rawQueryString req

attachRedirect :: Text -> Maybe Text -> ServerError -> ServerError
attachRedirect loginPath mReturn err =
  let baseLocation = loginPath
      location =
        maybe baseLocation (\rt -> baseLocation <> "?return_to=" <> encodeComponent rt) mReturn
      headerValue = encodeUtf8 location
      headers =
        [ ("Location", headerValue),
          ("HX-Redirect", headerValue)
        ]
          <> filter ((\name -> name /= "Location" && name /= "HX-Redirect") . fst) (errHeaders err)
   in err {errHeaders = headers}

textBody :: Text -> BL.ByteString
textBody = BL.fromStrict . encodeUtf8

encodeComponent :: Text -> Text
encodeComponent = decodeUtf8 . urlEncode True . encodeUtf8
