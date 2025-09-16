{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Starter.Server
  ( app
  , apiProxy
  , Api
  , HealthApi
  , OAuthApi
  , server
  , healthServer
  , HealthStatus (..)
  ) where

import Starter.Prelude

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable)
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad.IO.Class (liftIO)
import Servant
import Squeal.PostgreSQL (Jsonb (..))
import qualified Squeal.PostgreSQL as PQ

import Starter.Database.Connection (withAppConnection)
import Starter.Database.OAuth
  ( DbUserRow (..)
  , OAuthSessionRow (..)
  , deleteSession
  , insertLoginEvent
  , insertSession
  , selectSession
  , upsertUser
  )
import Starter.Env (AppEnv (..))
import Starter.OAuth.Types
  ( OAuthCallbackRequest (..)
  , OAuthCallbackResponse (..)
  , OAuthProfile (..)
  , OAuthStartRequest (..)
  , OAuthStartResponse (..)
  )

-- | API type definition for the Servant server.
type HealthApi = "health" :> Get '[JSON] HealthStatus

type OAuthApi =
  "oauth"
    :> Capture "provider" Text
    :> ( "start" :> ReqBody '[JSON] OAuthStartRequest :> Post '[JSON] OAuthStartResponse
          :<|> "callback" :> ReqBody '[JSON] OAuthCallbackRequest :> Post '[JSON] OAuthCallbackResponse
       )

type Api = HealthApi :<|> OAuthApi

data HealthStatus = HealthStatus
  { status :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance ToJSON HealthStatus where
  toJSON HealthStatus {status} = object ["status" .= status]

apiProxy :: Proxy Api
apiProxy = Proxy

app :: AppEnv -> Application
app env = serve apiProxy (server env)

server :: AppEnv -> Server Api
server env = healthServer :<|> oauthServer env

healthServer :: Server HealthApi
healthServer = pure (HealthStatus "ok")

oauthServer :: AppEnv -> Server OAuthApi
oauthServer env provider =
  startHandler env provider :<|> callbackHandler env provider

startHandler :: AppEnv -> Text -> OAuthStartRequest -> Handler OAuthStartResponse
startHandler env provider OAuthStartRequest {redirectUri = redirectUriValue, scopes} = do
  stateValue <- liftIO randomState
  codeVerifier <- liftIO randomVerifier
  let requestedScopes = Text.intercalate " " scopes
  liftIO $ withAppConnection (dbConfig env) $ do
    PQ.manipulateParams_ insertSession (stateValue, codeVerifier, provider, redirectUriValue, requestedScopes)
  let authorizationUrl = buildAuthorizationUrl provider redirectUriValue stateValue requestedScopes
  pure OAuthStartResponse {state = stateValue, codeVerifier, authorizationUrl}

callbackHandler :: AppEnv -> Text -> OAuthCallbackRequest -> Handler OAuthCallbackResponse
callbackHandler env provider request@OAuthCallbackRequest {state = callbackState, profile = callbackProfile} = do
  sessionRow <- liftIO (loadSession env callbackState) >>= maybe (throwError err400 {errBody = "unknown oauth state"}) pure
  when (sessionProvider sessionRow /= provider) $
    throwError err400 {errBody = "provider mismatch"}
  allowed <- liftIO (authorizeLogin env callbackProfile)
  now <- liftIO getCurrentTime
  dbUser <-
    liftIO (upsertUserRecord env provider callbackProfile allowed now)
      >>= maybe (throwError err500 {errBody = "failed to upsert user"}) pure
  liftIO $ recordLogin env dbUser provider request allowed now
  liftIO $ clearSession env callbackState
  let DbUserRow {userId = dbUserId} = dbUser
  pure
    OAuthCallbackResponse
      { allowed
      , redirectUri = sessionRedirectUri sessionRow
      , userId = dbUserId
      }

loadSession :: AppEnv -> Text -> IO (Maybe OAuthSessionRow)
loadSession env stateValue =
  withAppConnection (dbConfig env) $ do
    result <- PQ.executeParams selectSession (PQ.Only stateValue)
    rows <- PQ.getRows result
    pure (listToMaybe rows)

upsertUserRecord :: AppEnv -> Text -> OAuthProfile -> Bool -> UTCTime -> IO (Maybe DbUserRow)
upsertUserRecord env provider OAuthProfile {subject, email, displayName, avatarUrl} allowed now =
  withAppConnection (dbConfig env) $ do
    result <-
      PQ.manipulateParams
        upsertUser
        ( provider
        , subject
        , email
        , displayName
        , avatarUrl
        , allowed
        , Just now
        , now
        )
    rows <- PQ.getRows result
    pure (listToMaybe rows)

recordLogin :: AppEnv -> DbUserRow -> Text -> OAuthCallbackRequest -> Bool -> UTCTime -> IO ()
recordLogin env DbUserRow {userId = dbUserId} provider payload allowed now =
  withAppConnection (dbConfig env) $ do
    PQ.manipulateParams_
      insertLoginEvent
      ( dbUserId
      , provider
      , Jsonb (Aeson.toJSON payload)
      , allowed
      , now
      )

clearSession :: AppEnv -> Text -> IO ()
clearSession env stateValue =
  withAppConnection (dbConfig env) $
    PQ.manipulateParams_ deleteSession (PQ.Only stateValue)

randomState :: IO Text
randomState = UUID.toText <$> UUID.nextRandom

randomVerifier :: IO Text
randomVerifier = UUID.toText <$> UUID.nextRandom

buildAuthorizationUrl :: Text -> Text -> Text -> Text -> Text
buildAuthorizationUrl provider redirectUri state scopeText =
  "https://auth.example.com/" <> provider
    <> "?redirect_uri="
    <> redirectUri
    <> "&state="
    <> state
    <> scopesFragment
  where
    scopesFragment
      | Text.null scopeText = ""
      | otherwise = "&scope=" <> Text.replace " " "+" scopeText
