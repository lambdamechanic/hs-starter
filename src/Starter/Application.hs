module Starter.Application
  ( runApp
  , AppEnv (..)
  ) where

import Starter.Prelude

import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp (run)
import OpenTelemetry.Instrumentation.Servant (openTelemetryServantMiddleware)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace
  ( TracerProvider
  , initializeGlobalTracerProvider
  , shutdownTracerProvider
  )
import Starter.Server (app, apiProxy)
import System.Environment (lookupEnv, setEnv)
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket)

data AppEnv = AppEnv
  { appPort :: Int
  , otelServiceName :: Text
  , otelCollectorEndpoint :: Maybe String
  , otelCollectorHeaders :: Maybe String
  }

runApp :: IO ()
runApp = do
  env <- loadAppEnv
  withTracerProvider $ \tracerProvider -> do
    openTelemetryWaiMiddleware <- newOpenTelemetryWaiMiddleware
    let instrumentedApp =
          openTelemetryWaiMiddleware
            ( openTelemetryServantMiddleware tracerProvider apiProxy
                (app env)
            )
    run (appPort env) instrumentedApp

loadAppEnv :: IO AppEnv
loadAppEnv = do
  portEnv <- lookupEnv "PORT"
  serviceNameEnv <- lookupEnv "OTEL_SERVICE_NAME"
  collectorEndpoint <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  collectorHeaders <- lookupEnv "OTEL_EXPORTER_OTLP_HEADERS"
  let appPort = fromMaybe 8080 (portEnv >>= readMaybe)
      otelServiceName = maybe "hs-starter" Text.pack serviceNameEnv
  when (isNothing serviceNameEnv) $ setEnv "OTEL_SERVICE_NAME" (Text.unpack otelServiceName)
  pure
    AppEnv
      { appPort
      , otelServiceName
      , otelCollectorEndpoint = collectorEndpoint
      , otelCollectorHeaders = collectorHeaders
      }

withTracerProvider :: (TracerProvider -> IO a) -> IO a
withTracerProvider =
  bracket initializeGlobalTracerProvider shutdownTracerProvider
