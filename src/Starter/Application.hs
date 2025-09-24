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
import Starter.Auth.Firebase (loadFirebaseAuthFromEnv)
import Starter.Database.Connection (loadDbConfigFromEnv)
import Starter.Env (AppEnv (..))
import Starter.OAuth.Types (OAuthProfile)
import Starter.Prelude
import Starter.Server (apiProxy, app)
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket)

runApp :: IO ()
runApp = do
  env <- loadAppEnv
  withTracerProvider $ \tracerProvider -> do
    openTelemetryWaiMiddleware <- newOpenTelemetryWaiMiddleware
    let instrumentedApp =
          openTelemetryWaiMiddleware
            ( openTelemetryServantMiddleware
                tracerProvider
                apiProxy
                (app env)
            )
    run (appPort env) instrumentedApp

loadAppEnv :: IO AppEnv
loadAppEnv = do
  portEnv <- lookupEnv "PORT"
  serviceNameEnv <- lookupEnv "OTEL_SERVICE_NAME"
  collectorEndpoint <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  collectorHeaders <- lookupEnv "OTEL_EXPORTER_OTLP_HEADERS"
  dbConfig <- loadDbConfigFromEnv
  firebaseAuth <-
    loadFirebaseAuthFromEnv >>= either (ioError . userError . Text.unpack) pure
  let appPort = fromMaybe 8080 (portEnv >>= readMaybe)
      otelServiceName = maybe "hs-starter" Text.pack serviceNameEnv
  when (isNothing serviceNameEnv) $ setEnv "OTEL_SERVICE_NAME" (Text.unpack otelServiceName)
  pure
    AppEnv
      { appPort,
        otelServiceName,
        otelCollectorEndpoint = collectorEndpoint,
        otelCollectorHeaders = collectorHeaders,
        dbConfig,
        authorizeLogin = defaultAuthorizeLogin,
        firebaseAuth
      }

withTracerProvider :: (TracerProvider -> IO a) -> IO a
withTracerProvider =
  bracket initializeGlobalTracerProvider shutdownTracerProvider

-- | Default authorization hook that approves all logins.
defaultAuthorizeLogin :: OAuthProfile -> IO Bool
defaultAuthorizeLogin _profile = pure True
