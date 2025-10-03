{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Starter.Tests.Health
  ( spec,
  )
where

import Control.Exception (finally)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Object, Parser, parseEither, withObject, (.:))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Network.HTTP.Types (methodGet, status200)
import Network.Wai (Application, pathInfo, rawPathInfo, requestMethod)
import Network.Wai.Test
import Starter.Database.Connection (DbConfig (..))
import Starter.Auth.Firebase (firebaseAuthDisabled)
import Starter.Auth.Session (SessionConfig (..))
import qualified Data.ByteString.Char8 as BC
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Starter.Server (HealthCheckReport (..), HealthStatus (..), app)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Syd

testSessionConfig :: SessionConfig
testSessionConfig =
  SessionConfig
    { sessionSecret = BC.pack "test-session-secret",
      sessionCookieName = BC.pack "hs_test_session",
      sessionCookieMaxAge = 3600,
      sessionLoginPath = "/login"
    }

spec :: Spec
spec =
  describe "Health" $ do
    it "health returns 200 after DB init" healthOk

healthOk :: IO ()
healthOk = do
  mCfg <- withDockerPostgres
  case mCfg of
    Left msg -> do
      putStrLn ("docker postgres unavailable, skipping health test: " <> msg)
      pure ()
    Right (dbCfg, container) -> do
      let connectionCtx =
            unlines
              [ "docker postgres connection",
                "  host=" <> Text.unpack (dbHost dbCfg),
                "  port=" <> show (dbPort dbCfg),
                "  database=" <> Text.unpack (dbName dbCfg),
                "  user=" <> Text.unpack (dbUser dbCfg)
              ]
          cleanup = void (readProcessWithExitCode "docker" ["rm", "-f", container] "")
          env =
            AppEnv
              { appPort = 0,
                otelServiceName = "hs-starter-tests",
                otelCollectorEndpoint = Nothing,
                otelCollectorHeaders = Nothing,
                dbConfig = dbCfg,
                authorizeLogin = const (pure True),
                firebaseAuth = firebaseAuthDisabled,
                sessionConfig = testSessionConfig
              }
          application :: Application
          application = app env
      finally
        ( context connectionCtx $ do
            applyPgrollMigrations dbCfg
            res <-
              runSession
                (srequest (SRequest defaultRequest {requestMethod = methodGet, rawPathInfo = "/health", pathInfo = ["health"]} ""))
                application
            let code = simpleStatus res
            when (code /= status200) $
              expectationFailure
                ("/health did not return 200, got: " <> show code <> "; body=" <> show (simpleBody res))
            healthStatus <-
              case Aeson.eitherDecode (simpleBody res) of
                Left err ->
                  expectationFailure ("failed to decode health response: " <> err)
                Right payload -> pure payload
            validateHealthPayload env dbCfg healthStatus
        )
        cleanup

validateHealthPayload :: AppEnv -> DbConfig -> HealthStatus -> IO ()
validateHealthPayload env dbCfg payload = do
  payload.status `shouldBe` "ok"
  payload.service `shouldBe` otelServiceName env
  payload.version `shouldSatisfy` (not . Text.null)
  case HashMap.lookup "database" payload.checks of
    Nothing -> expectationFailure "health response missing database check"
    Just dbReport -> do
      dbReport.status `shouldBe` "ok"
      dbReport.durationMs `shouldSatisfy` (>= 0)
      case parseEither (databaseDetailsParser dbCfg) dbReport.details of
        Left err -> expectationFailure ("invalid database check details: " <> err)
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
applyPgrollMigrations :: DbConfig -> IO ()
applyPgrollMigrations DbConfig {dbHost, dbPort, dbName, dbUser, dbPassword} = do
  let url =
        "postgres://"
          <> Text.unpack dbUser
          <> maybe "" (\p -> ":" <> Text.unpack p) dbPassword
          <> "@"
          <> Text.unpack dbHost
          <> ":"
          <> show dbPort
          <> "/"
          <> Text.unpack dbName
          <> "?sslmode=disable"
      formatCommandContext label exitCode stdoutText stderrText =
        unlines
          [ label <> " exit=" <> show exitCode,
            "stdout:",
            indentBlock stdoutText,
            "stderr:",
            indentBlock stderrText
          ]
      indentBlock = unlines . fmap ("  " <>) . lines
  (vcode, vout, verr) <- readProcessWithExitCode "pgroll" ["--version"] ""
  context (formatCommandContext "pgroll --version" vcode vout verr) $
    vcode `shouldBe` ExitSuccess
  (icode, iout, ierr) <-
    readProcessWithExitCode
      "pgroll"
      ["init", "--postgres-url", url, "--schema", "public", "--pgroll-schema", "pgroll"]
      ""
  context (formatCommandContext "pgroll init" icode iout ierr) $
    icode `shouldBe` ExitSuccess
  (mcode, mout, merr) <-
    readProcessWithExitCode
      "pgroll"
      [ "migrate",
        "db/pgroll",
        "--postgres-url",
        url,
        "--schema",
        "public",
        "--pgroll-schema",
        "pgroll",
        "--complete"
      ]
      ""
  context (formatCommandContext "pgroll migrate" mcode mout merr) $
    mcode `shouldBe` ExitSuccess
