{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Starter.Tests.Health
  ( tests,
  )
where

import Control.Exception (finally)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Object, Parser, parseEither, withObject, (.:))
import Data.Maybe (fromMaybe)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Network.HTTP.Types (methodGet, status200)
import Network.Wai (Application, rawPathInfo, requestMethod, pathInfo)
import Network.Wai.Test
import Starter.Database.Connection (DbConfig (..))
import Starter.Auth.Firebase (firebaseAuthDisabled)
import Starter.Auth.Session (SessionConfig (..))
import qualified Data.ByteString.Char8 as BC
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Starter.Server (HealthCheckReport (..), HealthStatus (..), app)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

testSessionConfig :: SessionConfig
testSessionConfig =
  SessionConfig
    { sessionSecret = BC.pack "test-session-secret",
      sessionCookieName = BC.pack "hs_test_session",
      sessionCookieMaxAge = 3600,
      sessionLoginPath = "/login"
    }

tests :: TestTree
tests =
  testGroup
    "Health"
    [ testCase "health returns 200 after DB init" healthOk
    ]

healthOk :: IO ()
healthOk = do
  mCfg <- withDockerPostgres
  case mCfg of
    Left msg -> putStrLn ("docker postgres unavailable, skipping health test: " <> msg)
    Right (dbCfg, container) -> do
      mig <- applyPgrollMigrations dbCfg
      case mig of
        Left err -> do
          _ <- readProcessWithExitCode "docker" ["rm", "-f", container] ""
          assertFailure ("pgroll migrations failed: " <> err)
        Right () -> do
          let env =
                AppEnv
                  { appPort = 0,
                    otelServiceName = "hs-starter-tests",
                    otelCollectorEndpoint = Nothing,
                    otelCollectorHeaders = Nothing,
                    dbConfig = dbCfg,
                    authorizeLogin = const (pure True),
                    firebaseAuth = firebaseAuthDisabled,
                    sessionConfig = testSessionConfig,
                    frontendDir = "/opt/app/frontend"
                  }
              application :: Application
              application = app env
              cleanup = void (readProcessWithExitCode "docker" ["rm", "-f", container] "")
          finally
            ( do
                res <-
                  runSession
                    (srequest (SRequest defaultRequest {requestMethod = methodGet, rawPathInfo = "/health", pathInfo = ["health"]} ""))
                    application
                let code = simpleStatus res
                when (code /= status200) $
                  assertFailure
                    ("/health did not return 200, got: " <> show code <> "; body=" <> show (simpleBody res))
                healthStatus <-
                  case Aeson.eitherDecode (simpleBody res) of
                    Left err ->
                      assertFailure ("failed to decode health response: " <> err)
                    Right payload -> pure payload
                validateHealthPayload env dbCfg healthStatus
            )
            cleanup

validateHealthPayload :: AppEnv -> DbConfig -> HealthStatus -> IO ()
validateHealthPayload env dbCfg payload = do
  payload.status @?= "ok"
  payload.service @?= otelServiceName env
  assertBool "health version should be non-empty" (not (Text.null payload.version))
  case HashMap.lookup "database" payload.checks of
    Nothing -> assertFailure "health response missing database check"
    Just dbReport -> do
      dbReport.status @?= "ok"
      assertBool "database durationMs should be non-negative" (dbReport.durationMs >= 0)
      case parseEither (databaseDetailsParser dbCfg) dbReport.details of
        Left err -> assertFailure ("invalid database check details: " <> err)
        Right () -> pure ()

databaseDetailsParser :: DbConfig -> Aeson.Value -> Parser ()
databaseDetailsParser dbCfg =
  withObject "database details" $ \obj -> do
    messageText :: Text <- obj .: "message"
    when (Text.null messageText) (fail "message was empty")
    rowCount :: Int <- obj .: "rowCount"
    when (rowCount < 0) (fail "rowCount was negative")
    databaseValue <- obj .: "database"
    withObject "database connection info" (validateConnection dbCfg) databaseValue

validateConnection :: DbConfig -> Object -> Parser ()
validateConnection dbCfg dbObj = do
  host :: Text <- dbObj .: "host"
  port :: Int <- dbObj .: "port"
  name :: Text <- dbObj .: "database"
  user :: Text <- dbObj .: "user"
  when (host /= dbHost dbCfg) (fail "unexpected database host")
  when (port /= fromIntegral (dbPort dbCfg)) (fail "unexpected database port")
  when (name /= dbName dbCfg) (fail "unexpected database name")
  when (user /= dbUser dbCfg) (fail "unexpected database user")
  pure ()

-- | Start a Dockerized Postgres for tests, returning DbConfig.
withDockerPostgres :: IO (Either String (DbConfig, String))
withDockerPostgres = do
  (codeVer, _, _) <- readProcessWithExitCode "docker" ["--version"] ""
  if codeVer /= ExitSuccess
    then pure (Left "docker not found on PATH")
    else do
      let container = "hs-starter-test-db"
          image = fromMaybe "postgres:16-alpine" (Just "postgres:16-alpine")
      _ <- readProcessWithExitCode "docker" ["rm", "-f", container] ""
      (codeRun, _outRun, errRun) <- readProcessWithExitCode "docker" ["run", "-d", "--rm", "--name", container, "-e", "POSTGRES_PASSWORD=postgres", "-e", "POSTGRES_USER=postgres", "-e", "POSTGRES_DB=hs_starter", "-P", image] ""
      if codeRun /= ExitSuccess
        then pure (Left ("failed to start docker postgres: " <> errRun))
        else do
          -- Query mapped port
          (codePort, outPort, errPort) <- readProcessWithExitCode "docker" ["port", container, "5432/tcp"] ""
          portStr <- case codePort of
            ExitSuccess -> case reverse (takeWhile (/= '\n') outPort) of
              _ -> do
                let line = head (lines outPort)
                    p = reverse (takeWhile (/= ':') (reverse line))
                pure p
            _ -> pure ""
          if null portStr
            then do
              _ <- readProcessWithExitCode "docker" ["rm", "-f", container] ""
              pure (Left ("failed to get mapped port: " <> errPort))
            else do
              let port = read portStr :: Int
              let waitLoop 0 = pure ()
                  waitLoop n = do
                    (c, _, _) <- readProcessWithExitCode "docker" ["exec", container, "pg_isready", "-U", "postgres", "-d", "hs_starter"] ""
                    case c of
                      ExitSuccess -> pure ()
                      _ -> threadDelay 500000 >> waitLoop (n -1)
              waitLoop (40 :: Int)
              pure (Right ( DbConfig {dbHost = "127.0.0.1", dbPort = fromIntegral port, dbName = "hs_starter", dbUser = "postgres", dbPassword = Just "postgres"}
                           , container))

-- | Apply pgroll migrations against the given DbConfig.
applyPgrollMigrations :: DbConfig -> IO (Either String ())
applyPgrollMigrations DbConfig {dbHost, dbPort, dbName, dbUser, dbPassword} = do
  let url = "postgres://" <> Text.unpack dbUser
            <> maybe "" (\p -> ":" <> Text.unpack p) dbPassword
            <> "@" <> Text.unpack dbHost
            <> ":" <> show dbPort
            <> "/" <> Text.unpack dbName
            <> "?sslmode=disable"
  (vcode, vout, verr) <- readProcessWithExitCode "pgroll" ["--version"] ""
  putStrLn ("pgroll version: exit=" <> show vcode <> "; stdout=\n" <> vout <> "stderr=\n" <> verr)
  if vcode /= ExitSuccess
    then pure (Left "pgroll not found or failed to run")
    else do
      (icode, iout, ierr) <- readProcessWithExitCode "pgroll" ["init", "--postgres-url", url, "--schema", "public", "--pgroll-schema", "pgroll"] ""
      putStrLn ("pgroll init exit=" <> show icode)
      if icode /= ExitSuccess
        then pure (Left ("pgroll init failed:\nSTDOUT:\n" <> iout <> "\nSTDERR:\n" <> ierr))
        else do
          (mcode, mout, merr) <- readProcessWithExitCode "pgroll" ["migrate", "db/pgroll", "--postgres-url", url, "--schema", "public", "--pgroll-schema", "pgroll", "--complete"] ""
          putStrLn ("pgroll migrate exit=" <> show mcode)
          if mcode /= ExitSuccess
            then pure (Left ("pgroll migrate failed:\nSTDOUT:\n" <> mout <> "\nSTDERR:\n" <> merr))
            else pure (Right ())
