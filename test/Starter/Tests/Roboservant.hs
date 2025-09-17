{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Starter.Tests.Roboservant
  ( tests
  ) where

import Starter.Prelude

import Data.Maybe (isNothing)
import qualified Roboservant.Server as Robo
import Roboservant.Types (Atom (..), Breakdown)
import Roboservant.Types.Config (defaultConfig)
import Starter.Server (HealthApi, HealthStatus, healthServer)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import qualified Database.Postgres.Temp as Temp
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as Text
import Control.Exception (SomeException, catch)
import Starter.Env (AppEnv (..))
import Starter.Database.Connection (DbConfig (..))
import Starter.OAuth.Types (OAuthProfile)

-- | Run the Roboservant fuzzer against the health check API.
fuzzesHealthcheck :: IO ()
fuzzesHealthcheck = do
  -- Boot a temporary Postgres for exercising the health endpoint
  startResult <- catch (Right <$> Temp.start) (\(_ :: SomeException) -> pure (Left "exc"))
  case startResult of
    Left _ -> pure ()
    Right (Left _err) ->
      -- tmp-postgres reported unavailable; skip test
      pure ()
    Right (Right db) -> do
      let connStr = B8.unpack (Temp.toConnectionString db)
          kvs = map (break (=='=')) (words connStr)
          lookupKV :: String -> Maybe String
          lookupKV k = fmap (drop 1) (lookup k kvs)
          dbCfg = DbConfig
            { dbHost = maybe "localhost" Text.pack (lookupKV "host")
            , dbPort = maybe 5432 read (lookupKV "port")
            , dbName = maybe "postgres" Text.pack (lookupKV "dbname")
            , dbUser = maybe "postgres" Text.pack (lookupKV "user")
            , dbPassword = (
                case lookupKV "password" of
                  Nothing -> Nothing
                  Just "" -> Nothing
                  Just p -> Just (Text.pack p)
              )
            }
          env = AppEnv
            { appPort = 0
            , otelServiceName = "hs-starter-tests"
            , otelCollectorEndpoint = Nothing
            , otelCollectorHeaders = Nothing
            , dbConfig = dbCfg
            , authorizeLogin = const (pure True)
            }
      result <- Robo.fuzz @HealthApi (healthServer env) defaultConfig
      Temp.stop db
      assertBool "roboservant found a counterexample" (isNothing result)

tests :: TestTree
tests =
  testGroup
    "Roboservant"
    [testCase "covers healthcheck" fuzzesHealthcheck]

deriving via (Atom HealthStatus) instance Breakdown HealthStatus
