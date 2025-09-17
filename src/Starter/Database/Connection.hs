module Starter.Database.Connection
  ( DbConfig (..)
  , defaultDbConfig
  , loadDbConfigFromEnv
  , parseDatabaseUrl
  , renderConnectionString
  , withAppConnection
  ) where

import Starter.Prelude

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word16)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import qualified Database.PostgreSQL.Simple.URL as PGURL
import Squeal.PostgreSQL (PQ)
import qualified Squeal.PostgreSQL as PQ
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Starter.Database.Schema (AppDb)

data DbConfig = DbConfig
  { dbHost :: Text
  , dbPort :: Word16
  , dbName :: Text
  , dbUser :: Text
  , dbPassword :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Default configuration useful for local development and tests.
defaultDbConfig :: DbConfig
defaultDbConfig =
  DbConfig
    { dbHost = "localhost"
    , dbPort = 5432
    , dbName = "hs_starter"
    , dbUser = "postgres"
    , dbPassword = Nothing
    }

-- | Load connection parameters from environment variables, falling back to sensible defaults.
loadDbConfigFromEnv :: IO DbConfig
loadDbConfigFromEnv = do
  mUrl <- lookupEnvTextOptional "DATABASE_URL"
  case mUrl >>= parseDatabaseUrl of
    Just cfg -> pure cfg
    Nothing -> do
      host <- lookupEnvText "DB_HOST" (dbHost defaultDbConfig)
      port <- lookupEnvPort "DB_PORT" (dbPort defaultDbConfig)
      name <- lookupEnvText "DB_NAME" (dbName defaultDbConfig)
      user <- lookupEnvText "DB_USER" (dbUser defaultDbConfig)
      password <- lookupEnvTextOptional "DB_PASSWORD"
      pure
        DbConfig
          { dbHost = host
          , dbPort = port
          , dbName = name
          , dbUser = user
          , dbPassword = password
          }

-- | Parse a libpq-style DATABASE_URL into DbConfig.
-- Supports schemes: postgres:// or postgresql://
-- Examples:
--   postgres://user:pass@host:5432/dbname
--   postgresql://user@host/dbname
parseDatabaseUrl :: Text -> Maybe DbConfig
parseDatabaseUrl urlText = do
  ci <- PGURL.parseDatabaseUrl (Text.unpack urlText)
  let pw = case connectPassword ci of
             "" -> Nothing
             p   -> Just (Text.pack p)
  pure
    DbConfig
      { dbHost = Text.pack (connectHost ci)
      , dbPort = connectPort ci
      , dbName = Text.pack (connectDatabase ci)
      , dbUser = Text.pack (connectUser ci)
      , dbPassword = pw
      }

-- | Render the configuration into a libpq style connection string for Squeal.
renderConnectionString :: DbConfig -> ByteString
renderConnectionString config =
  let
    hostPart = "host=" <> dbHost config
    portPart = "port=" <> Text.pack (show (dbPort config))
    namePart = "dbname=" <> dbName config
    userPart = "user=" <> dbUser config
    passwordPart = ("password=" <>) <$> dbPassword config
  in
    Text.encodeUtf8 . Text.unwords $
      catMaybes
        [ Just hostPart
        , Just portPart
        , Just namePart
        , Just userPart
        , passwordPart
        ]

withAppConnection :: DbConfig -> PQ AppDb AppDb IO a -> IO a
withAppConnection config action =
  PQ.withConnection (renderConnectionString config) action

lookupEnvText :: String -> Text -> IO Text
lookupEnvText name fallback = do
  value <- lookupEnv name
  pure $ maybe fallback Text.pack value

lookupEnvTextOptional :: String -> IO (Maybe Text)
lookupEnvTextOptional name = do
  value <- lookupEnv name
  pure $ case value of
    Nothing -> Nothing
    Just val | null val -> Nothing
    Just val -> Just (Text.pack val)

lookupEnvPort :: String -> Word16 -> IO Word16
lookupEnvPort name fallback = do
  value <- lookupEnv name
  pure $ fromMaybe fallback (value >>= readMaybe)
