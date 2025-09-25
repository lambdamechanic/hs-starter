{-# OPTIONS_GHC -Wno-orphans #-}
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
    appApiProxy,
    Api,
    HealthApi,
    FirebaseConfigApi,
    SessionApi,
    PrivateApi,
    server,
    healthServer,
    HealthStatus (..),
    HealthCheckReport (..),
    FirebaseClientConfig (..),
    UserProfileResponse (..)
  )
where

import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value, encode, object, (.=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import OpenTelemetry.Instrumentation.Servant.Internal (HasEndpoint (getEndpoint))
import Servant
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import Squeal.PostgreSQL (Jsonb (..))
import Squeal.PostgreSQL qualified as PQ
import Starter.Auth.Firebase (FirebaseAuth (..), FirebaseUser (..), toServerError)
import Starter.Auth.Session (SessionConfig (..), SessionUser (..), mkSessionCookie, sessionContext)
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.Database.Users (DbUserRow (..), insertLoginEvent, selectUserCount, upsertUser)
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Paths_hs_starter qualified as Paths
import Web.Cookie (defaultSetCookie, renderSetCookie, sameSiteLax, setCookieExpires, setCookieHttpOnly, setCookieMaxAge, setCookieName, setCookiePath, setCookieSameSite, setCookieSecure, setCookieValue)

-- | API type definitions.
data HealthStatus = HealthStatus
  { status :: Text,
    timestamp :: UTCTime,
    service :: Text,
    version :: Text,
    checks :: HashMap.HashMap Text HealthCheckReport
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data HealthCheckReport = HealthCheckReport
  { status :: Text,
    observedAt :: UTCTime,
    durationMs :: Double,
    details :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SessionExchangeRequest = SessionExchangeRequest
  { serIdToken :: Text,
    serReturnTo :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SessionExchangeRequest where
  parseJSON =
    Aeson.withObject "SessionExchangeRequest" $ \obj -> do
      token <- obj .: "idToken"
      returnTo <- obj .:? "return_to"
      pure SessionExchangeRequest {serIdToken = token, serReturnTo = returnTo}

data SessionExchangeResponse = SessionExchangeResponse
  { redirect :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data SessionLogoutResponse = SessionLogoutResponse
  { ok :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ErrorResponse = ErrorResponse
  { error :: ErrorBody
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Top-level API definition.
type HealthApi = "health" :> Get '[JSON] HealthStatus

type FirebaseConfigApi = "firebase" :> "config" :> Get '[JSON] FirebaseClientConfig

type SessionApi =
  "session"
    :> ( "exchange"
          :> ReqBody '[JSON] SessionExchangeRequest
          :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] SessionExchangeResponse)
       :<|> "logout"
          :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] SessionLogoutResponse)
       )

type PrivateApi =
  AuthProtect "session"
    :> "me"
    :> Get '[JSON] UserProfileResponse

-- | Combined API.
type Api = HealthApi :<|> FirebaseConfigApi :<|> SessionApi :<|> PrivateApi

-- | Combined API with static assets served via Wai's Raw handler.
type AppApi = Api :<|> Raw


data ErrorBody = ErrorBody
  { code :: Text,
    message :: Text,
    details :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)


data FirebaseClientConfig = FirebaseClientConfig
  { apiKey :: Text,
    authDomain :: Text,
    projectId :: Text,
    appId :: Maybe Text,
    messagingSenderId :: Maybe Text,
    storageBucket :: Maybe Text,
    measurementId :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data UserProfileResponse = UserProfileResponse
  { firebase :: FirebaseUser,
    allowed :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

instance HasEndpoint api => HasEndpoint (AuthProtect tag :> api) where
  getEndpoint _ req = getEndpoint (Proxy :: Proxy api) req

apiProxy :: Proxy Api
apiProxy = Proxy

appApiProxy :: Proxy AppApi
appApiProxy = Proxy

app :: AppEnv -> Application
app env =
  serveWithContext
    appApiProxy
    (sessionContext (sessionConfig env))
    (server env :<|> staticServer env)

server :: AppEnv -> Server Api
server env =
  healthServer env
    :<|> firebaseConfigServer env
    :<|> sessionServer env
    :<|> privateServer env

staticServer :: AppEnv -> Server Raw
staticServer env = serveDirectoryWebApp (frontendDir env)

firebaseConfigServer :: AppEnv -> Server FirebaseConfigApi
firebaseConfigServer env =
  let FirebaseAuth
        { firebaseProjectId = projectId,
          firebaseApiKey = apiKey,
          firebaseAuthDomain = authDomain,
          firebaseAppId = appId,
          firebaseMessagingSenderId = messagingSenderId,
          firebaseStorageBucket = storageBucket,
          firebaseMeasurementId = measurementId
        } = firebaseAuth env
   in pure
        FirebaseClientConfig
          { apiKey = apiKey,
            authDomain = authDomain,
            projectId = projectId,
            appId = appId,
            messagingSenderId = messagingSenderId,
            storageBucket = storageBucket,
            measurementId = measurementId
          }

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

sessionServer :: AppEnv -> Server SessionApi
sessionServer env = sessionExchangeHandler env :<|> sessionLogoutHandler env

privateServer :: AppEnv -> Server PrivateApi
privateServer env (SessionUser user) = meHandler env user

sessionExchangeHandler :: AppEnv -> SessionExchangeRequest -> Handler (Headers '[Header "Set-Cookie" Text] SessionExchangeResponse)
sessionExchangeHandler env SessionExchangeRequest {serIdToken, serReturnTo} = do
  verificationResult <- liftIO (firebaseVerifyIdToken (firebaseAuth env) serIdToken)
  firebaseUser <- either (throwError . toServerError) pure verificationResult
  now <- liftIO getCurrentTime
  allowed <- liftIO (authorizeLogin env firebaseUser)
  dbUser <-
    liftIO (upsertFirebaseUser env firebaseUser allowed now)
      >>= maybe (throwError err500 {errBody = "failed to upsert user"}) pure
  liftIO $ recordFirebaseLogin env dbUser firebaseUser allowed now
  unless allowed $ throwError (forbiddenLogin firebaseUser)
  let cookie = mkSessionCookie (sessionConfig env) now firebaseUser
      cookieHeader =
        decodeUtf8
          ( BL.toStrict
              (toLazyByteString (renderSetCookie cookie))
          )
      redirectTarget = resolveReturnTo serReturnTo
  pure $ addHeader cookieHeader SessionExchangeResponse {redirect = redirectTarget}

sessionLogoutHandler :: AppEnv -> Handler (Headers '[Header "Set-Cookie" Text] SessionLogoutResponse)
sessionLogoutHandler env = do
  now <- liftIO getCurrentTime
  let SessionConfig {sessionCookieName} = sessionConfig env
      expiredCookie =
        defaultSetCookie
          { setCookieName = sessionCookieName,
            setCookieValue = "",
            setCookiePath = Just "/",
            setCookieExpires = Just (addUTCTime (-3600) now),
            setCookieMaxAge = Just 0,
            setCookieHttpOnly = True,
            setCookieSecure = True,
            setCookieSameSite = Just sameSiteLax
          }
      cookieHeader =
        decodeUtf8
          ( BL.toStrict
              (toLazyByteString (renderSetCookie expiredCookie))
          )
  pure $ addHeader cookieHeader SessionLogoutResponse {ok = True}

meHandler :: AppEnv -> FirebaseUser -> Handler UserProfileResponse
meHandler env user = do
  now <- liftIO getCurrentTime
  allowed <- liftIO (authorizeLogin env user)
  dbUser <-
    liftIO (upsertFirebaseUser env user allowed now)
      >>= maybe (throwError err500 {errBody = "failed to upsert user"}) pure
  liftIO $ recordFirebaseLogin env dbUser user allowed now
  if allowed
    then pure (UserProfileResponse {firebase = user, allowed = userAllowed dbUser})
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

resolveReturnTo :: Maybe Text -> Text
resolveReturnTo = maybe "/" sanitize
  where
    sanitize candidate =
      let trimmed = Text.takeWhile (/= '#') candidate
          target = if Text.null trimmed then "/" else trimmed
       in if Text.isPrefixOf "//" target || Text.head target /= '/'
            then "/"
            else target

authorizeLogin :: AppEnv -> FirebaseUser -> IO Bool
authorizeLogin AppEnv {firebaseAuth = FirebaseAuth {firebaseProjectId = projectId}} FirebaseUser {issuer = issuerValue} =
  pure (issuerValue == "https://securetoken.google.com/" <> projectId)

