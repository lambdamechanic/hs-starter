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
            applyPgrollMigrations container dbCfg
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
applyPgrollMigrations :: String -> DbConfig -> IO ()
applyPgrollMigrations container DbConfig {dbHost, dbPort, dbName, dbUser, dbPassword} = do
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
      maxAttempts = 3 :: Int
      retryDelayMicros = 500000
      formatCommandContext label attempt exitCode stdoutText stderrText = do
        dockerSection <-
          if exitCode == ExitSuccess
            then pure ""
            else do
              (dcode, dout, derr) <- readProcessWithExitCode "docker" ["logs", container] ""
              let dockerHeader =
                    unwords
                      [ "docker logs",
                        container,
                        "exit=" <> show dcode,
                        "(attempt",
                        show attempt <> ")"
                      ]
              pure $
                "\n"
                  <> unlines
                    [ dockerHeader,
                      "stdout:",
                      indentBlock dout,
                      "stderr:",
                      indentBlock derr
                    ]
        let baseContext =
              unlines
                [ label <> " (attempt " <> show attempt <> ") exit=" <> show exitCode,
                  "stdout:",
                  indentBlock stdoutText,
                  "stderr:",
                  indentBlock stderrText
                ]
        pure (baseContext <> dockerSection)
      indentBlock = unlines . fmap ("  " <>) . lines
      runWithRetries label args = go 1 []
        where
          go attempt acc = do
            (code, out, err) <- readProcessWithExitCode "pgroll" args ""
            ctx <- formatCommandContext label attempt code out err
            let acc' = acc ++ [ctx]
            if code == ExitSuccess || attempt >= maxAttempts
              then pure (acc', code)
              else threadDelay retryDelayMicros >> go (attempt + 1) acc'
  (versionContexts, vcode) <- runWithRetries "pgroll --version" ["--version"]
  case versionContexts of
    [] -> expectationFailure "pgroll --version produced no context"
    ctxs -> do
      mapM_ (\ctx -> context ctx (pure ())) (init ctxs)
      context (last ctxs) $ vcode `shouldBe` ExitSuccess
  (initContexts, icode) <-
    runWithRetries
      "pgroll init"
      ["init", "--postgres-url", url, "--schema", "public", "--pgroll-schema", "pgroll"]
  case initContexts of
    [] -> expectationFailure "pgroll init produced no context"
    ctxs -> do
      mapM_ (\ctx -> context ctx (pure ())) (init ctxs)
      context (last ctxs) $ icode `shouldBe` ExitSuccess
  (migrateContexts, mcode) <-
    runWithRetries
      "pgroll migrate"
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
  case migrateContexts of
    [] -> expectationFailure "pgroll migrate produced no context"
    ctxs -> do
      mapM_ (\ctx -> context ctx (pure ())) (init ctxs)
      context (last ctxs) $ mcode `shouldBe` ExitSuccess
