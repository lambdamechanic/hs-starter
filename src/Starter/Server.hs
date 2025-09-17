{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Starter.Server
  ( app,
    apiProxy,
    Api,
    HealthApi,
    OAuthApi,
    server,
    healthServer,
    HealthStatus (..),
    HealthCheckReport (..),
  )
where

import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Version (showVersion)
import Servant
import Squeal.PostgreSQL (Jsonb (..))
import Squeal.PostgreSQL qualified as PQ
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.Database.OAuth
  ( DbUserRow (..),
    OAuthSessionRow (..),
    deleteSession,
    insertLoginEvent,
    insertSession,
    selectSession,
    selectUserCount,
    upsertUser,
  )
import Starter.Env (AppEnv (..))
import Starter.OAuth.Types
  ( OAuthCallbackRequest (..),
    OAuthCallbackResponse (..),
    OAuthProfile (..),
    OAuthStartRequest (..),
    OAuthStartResponse (..),
  )
import Starter.Prelude
import Paths_hs_starter qualified as Paths

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
  { status :: Text,
    timestamp :: UTCTime,
    service :: Text,
    version :: Text,
    checks :: HashMap.HashMap Text HealthCheckReport
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HealthCheckReport = HealthCheckReport
  { status :: Text,
    observedAt :: UTCTime,
    durationMs :: Double,
    details :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ErrorResponse = ErrorResponse
  { error :: ErrorBody
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ErrorBody = ErrorBody
  { code :: Text,
    message :: Text,
    details :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

apiProxy :: Proxy Api
apiProxy = Proxy

app :: AppEnv -> Application
app env = serve apiProxy (server env)

server :: AppEnv -> Server Api
server env = healthServer env :<|> oauthServer env

healthServer :: AppEnv -> Server HealthApi
healthServer env = do
  timestamp <- liftIO getCurrentTime
  let dbCfg = dbConfig env
      versionText = Text.pack (showVersion Paths.version)
      databaseDetails =
        object
          [ "host" .= dbHost dbCfg,
            "port" .= dbPort dbCfg,
            "database" .= dbName dbCfg,
            "user" .= dbUser dbCfg
          ]
  checkStartedAt <- liftIO getCurrentTime
  dbResult <-
    liftIO . try @SomeException $ withAppConnection dbCfg $ do
      result <- PQ.executeParams selectUserCount ()
      rows <- PQ.getRows result
      case rows of
        [PQ.Only count] -> pure count
        unexpected ->
          liftIO
            $ throwIO
              ( userError
                  ("unexpected result from selectUserCount: " <> show unexpected)
              )
  checkCompletedAt <- liftIO getCurrentTime
  let durationMs :: Double
      durationMs = realToFrac (diffUTCTime checkCompletedAt checkStartedAt) * 1000
  case dbResult of
    Left (ex :: SomeException) ->
      let errorPayload =
            ErrorResponse
              { error =
                  ErrorBody
                    { code = "DATABASE_CHECK_FAILED",
                      message = "Database connectivity check failed",
                      details =
                        object
                          [ "timestamp" .= timestamp,
                            "service" .= otelServiceName env,
                            "version" .= versionText,
                            "observedAt" .= checkCompletedAt,
                            "durationMs" .= durationMs,
                            "database" .= databaseDetails,
                            "exception" .= displayException ex
                          ]
                    }
              }
       in throwError
            err500
              { errBody = encode errorPayload,
                errHeaders = [("Content-Type", "application/json; charset=utf-8")]
              }
    Right count ->
      let databaseReport =
            HealthCheckReport
              { status = "ok",
                observedAt = checkCompletedAt,
                durationMs = durationMs,
                details =
                  object
                    [ "message" .= ("Successfully queried user count" :: Text),
                      "rowCount" .= count,
                      "database" .= databaseDetails
                    ]
              }
       in pure
            HealthStatus
              { status = "ok",
                timestamp = timestamp,
                service = otelServiceName env,
                version = versionText,
                checks = HashMap.fromList [("database", databaseReport)]
              }

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
  when (sessionProvider sessionRow /= provider)
    $ throwError err400 {errBody = "provider mismatch"}
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
      { allowed,
        redirectUri = sessionRedirectUri sessionRow,
        userId = dbUserId
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
        ( provider,
          subject,
          email,
          displayName,
          avatarUrl,
          allowed,
          Just now,
          now
        )
    rows <- PQ.getRows result
    pure (listToMaybe rows)

recordLogin :: AppEnv -> DbUserRow -> Text -> OAuthCallbackRequest -> Bool -> UTCTime -> IO ()
recordLogin env DbUserRow {userId = dbUserId} provider payload allowed now =
  withAppConnection (dbConfig env) $ do
    PQ.manipulateParams_
      insertLoginEvent
      ( dbUserId,
        provider,
        Jsonb (Aeson.toJSON payload),
        allowed,
        now
      )

clearSession :: AppEnv -> Text -> IO ()
clearSession env stateValue =
  withAppConnection (dbConfig env)
    $ PQ.manipulateParams_ deleteSession (PQ.Only stateValue)

randomState :: IO Text
randomState = UUID.toText <$> UUID.nextRandom

randomVerifier :: IO Text
randomVerifier = UUID.toText <$> UUID.nextRandom

buildAuthorizationUrl :: Text -> Text -> Text -> Text -> Text
buildAuthorizationUrl provider redirectUri state scopeText =
  "https://auth.example.com/"
    <> provider
    <> "?redirect_uri="
    <> redirectUri
    <> "&state="
    <> state
    <> scopesFragment
  where
    scopesFragment
      | Text.null scopeText = ""
      | otherwise = "&scope=" <> Text.replace " " "+" scopeText
