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
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Base64.Types (extractBase64)
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
loadSessionConfigFromEnv = runExceptT $ do
  secretText <- requireEnvNonEmpty "SESSION_SECRET" "SESSION_SECRET is not set" "SESSION_SECRET cannot be empty"
  cookieNameEnv <- liftIO (lookupEnv "SESSION_COOKIE_NAME")
  let cookieName =
        maybe defaultCookieName id $ do
          name <- encodeUtf8 . Text.pack <$> cookieNameEnv
          guard (not (BS.null name))
          pure name
      defaultCookieName = "hs_session"
  maxAgeEnv <- liftIO (lookupEnv "SESSION_COOKIE_MAX_AGE_SECONDS")
  let maxAgeSeconds =
        maybe defaultSessionMaxAge id $ do
          raw <- maxAgeEnv
          seconds <- readMaybe raw :: Maybe Integer
          guard (seconds > 0)
          pure seconds
  loginPathEnv <- liftIO (lookupEnv "SESSION_LOGIN_PATH")
  let loginPath = maybe "/login" Text.pack loginPathEnv
  pure
    SessionConfig
      { sessionSecret = encodeUtf8 secretText,
        sessionCookieName = cookieName,
        sessionCookieMaxAge = fromInteger maxAgeSeconds,
        sessionLoginPath = loginPath
      }
  where
    defaultSessionMaxAge :: Integer
    defaultSessionMaxAge = 60 * 60 * 24 * 7 -- one week

-- | Produce a 'Set-Cookie' header for the provided Firebase user.
mkSessionCookie :: SessionConfig -> UTCTime -> FirebaseUser -> SetCookie
mkSessionCookie SessionConfig {sessionSecret, sessionCookieName, sessionCookieMaxAge} now user =
  let payloadRaw = BL.toStrict (Aeson.encode user)
      payload = extractBase64 (B64.encodeBase64' payloadRaw)
      mac = hmac sessionSecret payload :: HMAC SHA256
      signature = extractBase64 (B64.encodeBase64' (BA.convert mac :: ByteString))
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
        missing = redirectToLogin config req "Missing session cookie"
    token <- maybe (Except.throwError missing) pure (lookup (sessionCookieName config) cookies)
    user <- either Except.throwError pure (decodeSessionCookie config token)
    pure (SessionUser user)

-- | Parse and verify a cookie.
decodeSessionCookie :: SessionConfig -> ByteString -> Either ServerError FirebaseUser
decodeSessionCookie SessionConfig {sessionSecret, sessionLoginPath} raw =
  case BS.break (== 46) raw of -- '.'
    (payloadB64, rest)
      | BS.null payloadB64 || BS.null rest -> Left invalidSession
      | otherwise ->
          let signatureB64 = BS.tail rest
           in do
                payload <- first (const invalidSession) (B64.decodeBase64Untyped payloadB64)
                signature <- first (const invalidSession) (B64.decodeBase64Untyped signatureB64)
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
      either (const Nothing) (
        \txt -> do
          guard (Text.isPrefixOf "/" txt && not (Text.isPrefixOf "//" txt))
          pure txt
      )
      (decodeUtf8' raw)
  where
    raw = rawPathInfo req <> rawQueryString req

requireEnv :: String -> Text -> ExceptT Text IO Text
requireEnv name missingMsg = do
  value <- liftIO (lookupEnv name)
  maybe (Except.throwError missingMsg) (pure . Text.pack) value

requireEnvNonEmpty :: String -> Text -> Text -> ExceptT Text IO Text
requireEnvNonEmpty name missingMsg emptyMsg = do
  value <- requireEnv name missingMsg
  if Text.null value then Except.throwError emptyMsg else pure value

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
