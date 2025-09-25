module Starter.Application
  ( runApp,
    AppEnv (..),
  )
where

import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Network.Wai.Handler.Warp (run)
import OpenTelemetry.Instrumentation.Servant (openTelemetryServantMiddleware)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace
  ( TracerProvider,
    initializeGlobalTracerProvider,
    shutdownTracerProvider,
  )
import Starter.Auth.Firebase (FirebaseUser, loadFirebaseAuthFromEnv)
import Starter.Auth.Session (loadSessionConfigFromEnv)
import Starter.Database.Connection (loadDbConfigFromEnv)
import Starter.Env (AppEnv (..))
import System.Directory (doesDirectoryExist)
import Starter.Prelude
import Starter.Server (app, appApiProxy)
import System.Environment (lookupEnv, setEnv)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket)

runApp :: IO ()
runApp = do
  logStage "Loading application environment"
  env <- loadAppEnv
  logStage "Initializing tracer provider"
  withTracerProvider $ \tracerProvider -> do
    logStage "Creating OpenTelemetry middleware"
    openTelemetryWaiMiddleware <- newOpenTelemetryWaiMiddleware
    let instrumentedApp =
          openTelemetryWaiMiddleware
            ( openTelemetryServantMiddleware
                tracerProvider
                appApiProxy
                (app env)
            )
    logStage ("Starting Warp on port " <> show (appPort env))
    run (appPort env) instrumentedApp

loadAppEnv :: IO AppEnv
loadAppEnv = do
  portEnv <- lookupEnv "PORT"
  serviceNameEnv <- lookupEnv "OTEL_SERVICE_NAME"
  collectorEndpoint <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  collectorHeaders <- lookupEnv "OTEL_EXPORTER_OTLP_HEADERS"
  frontendDirEnv <- lookupEnv "FRONTEND_DIST_DIR"
  localFrontendExists <- doesDirectoryExist "frontend/build"
  logStage "Loading database configuration"
  dbConfig <- loadDbConfigFromEnv
  logStage "Loading Firebase configuration"
  firebaseAuth <-
    loadFirebaseAuthFromEnv >>= either (ioError . userError . Text.unpack) pure
  logStage "Loading session configuration"
  sessionConfig <-
    loadSessionConfigFromEnv >>= either (ioError . userError . Text.unpack) pure
  let appPort = fromMaybe 8080 (portEnv >>= readMaybe)
      otelServiceName = maybe "hs-starter" Text.pack serviceNameEnv
  when (isNothing serviceNameEnv) $ setEnv "OTEL_SERVICE_NAME" (Text.unpack otelServiceName)
  logStage "Application environment loaded"
  pure
    AppEnv
      { appPort,
        otelServiceName,
        otelCollectorEndpoint = collectorEndpoint,
        otelCollectorHeaders = collectorHeaders,
        dbConfig,
        authorizeLogin = defaultAuthorizeLogin,
        firebaseAuth,
        sessionConfig,
        frontendDir =
          let defaultFrontend = if localFrontendExists then "frontend/build" else "/opt/app/frontend"
           in fromMaybe defaultFrontend frontendDirEnv
      }

withTracerProvider :: (TracerProvider -> IO a) -> IO a
withTracerProvider =
  bracket initializeGlobalTracerProvider shutdownTracerProvider

-- | Default authorization hook that approves all logins.
defaultAuthorizeLogin :: FirebaseUser -> IO Bool
defaultAuthorizeLogin _profile = pure True

logStage :: String -> IO ()
logStage msg = do
  putStrLn ("[startup] " <> msg)
  hFlush stdout
