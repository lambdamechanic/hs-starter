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
    PrivateApi,
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
import Data.Version (showVersion)
import Servant
import Squeal.PostgreSQL (Jsonb (..))
import Squeal.PostgreSQL qualified as PQ
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.Database.Users
  ( DbUserRow (..),
    insertLoginEvent,
    selectUserCount,
    upsertUser,
  )
import Starter.Auth.Firebase (FirebaseUser (..), Protected, firebaseContext)
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Paths_hs_starter qualified as Paths

-- | API type definition for the Servant server.
type HealthApi = "health" :> Get '[JSON] HealthStatus

type PrivateApi =
  Protected
    ( "me" :> Get '[JSON] FirebaseUser
    )

type Api = HealthApi :<|> PrivateApi

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
app env = serveWithContext apiProxy (firebaseContext (firebaseAuth env)) (server env)

server :: AppEnv -> Server Api
server env = healthServer env :<|> privateServer env

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

privateServer :: AppEnv -> Server PrivateApi
privateServer env user = meHandler env user

meHandler :: AppEnv -> FirebaseUser -> Handler FirebaseUser
meHandler env user = do
  now <- liftIO getCurrentTime
  allowed <- liftIO (authorizeLogin env user)
  dbUser <-
    liftIO (upsertFirebaseUser env user allowed now)
      >>= maybe (throwError err500 {errBody = "failed to upsert user"}) pure
  liftIO $ recordFirebaseLogin env dbUser user allowed now
  if allowed
    then pure user
    else throwError (forbiddenLogin user)

upsertFirebaseUser :: AppEnv -> FirebaseUser -> Bool -> UTCTime -> IO (Maybe DbUserRow)
upsertFirebaseUser env FirebaseUser {uid = subject, issuer = issuerValue, email = userEmail, name = displayName, picture = avatarUrl} allowed now =
  withAppConnection (dbConfig env) $ do
    result <-
      PQ.manipulateParams
        upsertUser
        ( issuerValue,
          subject,
          userEmail,
          displayName,
          avatarUrl,
          allowed,
          Just now,
          now
        )
    rows <- PQ.getRows result
    pure (listToMaybe rows)

recordFirebaseLogin :: AppEnv -> DbUserRow -> FirebaseUser -> Bool -> UTCTime -> IO ()
recordFirebaseLogin env DbUserRow {userId = dbUserId} firebaseUser@FirebaseUser {issuer = issuerValue} allowed now =
  withAppConnection (dbConfig env)
    $ PQ.manipulateParams_
      insertLoginEvent
      ( dbUserId,
        issuerValue,
        Jsonb (Aeson.toJSON firebaseUser),
        allowed,
        now
      )

forbiddenLogin :: FirebaseUser -> ServerError
forbiddenLogin FirebaseUser {uid = subject, issuer = issuerValue} =
  attachJson err403
    { errBody =
        encode
          ErrorResponse
            { error =
                ErrorBody
                  { code = "LOGIN_NOT_ALLOWED",
                    message = "User is not permitted to log in",
                    details =
                      object
                        [ "uid" .= subject,
                          "issuer" .= issuerValue
                        ]
                  }
            }
    }

attachJson :: ServerError -> ServerError
attachJson err =
  err {errHeaders = ("Content-Type", "application/json; charset=utf-8") : filter ((/= "Content-Type") . fst) (errHeaders err)}
