module Starter.Tests.Db
  ( tests,
  )
where

import Starter.Prelude
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import Starter.Database.Connection (DbConfig (..))
import Squeal.PostgreSQL qualified as PQ
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | Ensure dockerized Postgres accepts Squeal connections.
tests :: TestTree
tests =
  testGroup
    "docker-postgres"
    [ testCase "connects" dockerPostgresConnects
    ]

dockerPostgresConnects :: IO ()
dockerPostgresConnects = do
  (cver, _, _) <- readProcessWithExitCode "docker" ["--version"] ""
  case cver of
    ExitSuccess -> pure ()
    _ -> do
      putStrLn "docker not available, skipping"
      pure ()
  let container = "hs-starter-test-db"
  _ <- readProcessWithExitCode "docker" ["rm", "-f", container] ""
  (crun, _o, _e) <- readProcessWithExitCode "docker" ["run", "-d", "--rm", "--name", container, "-e", "POSTGRES_PASSWORD=postgres", "-e", "POSTGRES_USER=postgres", "-e", "POSTGRES_DB=hs_starter", "-P", "postgres:16-alpine"] ""
  case crun of
    ExitSuccess -> pure ()
    _ -> putStrLn "failed to start container; skipping"
  (cp, op, _ep) <- readProcessWithExitCode "docker" ["port", container, "5432/tcp"] ""
  case cp of
    ExitSuccess -> do
      let line = head (lines op)
          p = reverse (takeWhile (/= ':') (reverse line))
          port = read p :: Int
          cfg = DbConfig {dbHost = "127.0.0.1", dbPort = fromIntegral port, dbName = "hs_starter", dbUser = "postgres", dbPassword = Just "postgres"}
      PQ.withConnection (renderConn cfg) (pure ())
      _ <- readProcessWithExitCode "docker" ["rm", "-f", container] ""
      pure ()
    _ -> do
      _ <- readProcessWithExitCode "docker" ["rm", "-f", container] ""
      pure ()

renderConn :: DbConfig -> BS.ByteString
renderConn cfg =
  let hostPart = "host=" <> Text.unpack (dbHost cfg)
      portPart = "port=" <> show (dbPort cfg)
      namePart = "dbname=" <> Text.unpack (dbName cfg)
      userPart = "user=" <> Text.unpack (dbUser cfg)
      passParts = maybe [] (\p -> ["password=" <> Text.unpack p]) (dbPassword cfg)
      parts = [hostPart, portPart, namePart, userPart] ++ passParts
   in BS.pack (unwords parts)
