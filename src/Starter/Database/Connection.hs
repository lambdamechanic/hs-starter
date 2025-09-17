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
import Data.Char (digitToInt)
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
parseDatabaseUrl url = do
  let stripScheme t
        | Text.isPrefixOf "postgresql://" (Text.toLower t) = Just (Text.drop 13 t)
        | Text.isPrefixOf "postgres://" (Text.toLower t) = Just (Text.drop 11 t)
        | otherwise = Nothing
  rest <- stripScheme url
  let (credPart, hostPart0) =
        case Text.breakOnEnd "@" rest of
          (pre, post) | Text.null pre -> (Nothing, rest)
          (pre, post) ->
            let pre' = Text.dropEnd 1 pre -- drop '@'
            in if Text.null pre'
                 then (Nothing, post)
                 else (Just pre', post)
      (userT, passT) =
        case credPart of
          Nothing -> (Nothing, Nothing)
          Just cp ->
            case Text.breakOn ":" cp of
              (u, p) | Text.null p -> (Just (pctDecode u), Nothing)
              (u, p) -> (Just (pctDecode u), Just (pctDecode (Text.drop 1 p)))
      -- hostPart0 starts with host[:port]/path?query
      (hostPortT, pathQ) = case Text.breakOn "/" hostPart0 of
        (hp, rest') | Text.null rest' -> (hp, Text.empty)
        (hp, rest') -> (hp, Text.drop 1 rest')
      (hostT, portT) =
        if Text.isPrefixOf "[" hostPortT
          then -- IPv6 literal [::1]:5432
            let afterL = Text.drop 1 hostPortT
                (h, rest') = Text.breakOn "]" afterL
                rest'' = Text.drop 1 rest'
            in case Text.uncons rest'' of
                 Just (':', p) -> (h, Just p)
                 _ -> (h, Nothing)
          else case Text.breakOn ":" hostPortT of
            (h, p) | Text.null p -> (h, Nothing)
            (h, p) -> (h, Just (Text.drop 1 p))
      dbNameT = case Text.breakOn "?" pathQ of
        (p, _q) -> p
      host' = pctDecode hostT
      user' = fmap pctDecode userT
      pass' = fmap pctDecode passT
      mPort = portT >>= readWord16
      port' = fromMaybe 5432 mPort
  guard (not (Text.null dbNameT))
  userText <- user'
  let cfg =
        DbConfig
          { dbHost = host'
          , dbPort = port'
          , dbName = pctDecode dbNameT
          , dbUser = userText
          , dbPassword = pass'
          }
  pure cfg

readWord16 :: Text -> Maybe Word16
readWord16 t = readMaybe (Text.unpack t)

-- Percent-decode a URL component (minimal implementation).
pctDecode :: Text -> Text
pctDecode = Text.pack . go . Text.unpack
  where
    hexVal c
      | '0' <= c && c <= '9' = Just (digitToInt c)
      | 'a' <= c && c <= 'f' = Just (10 + digitToInt c - digitToInt 'a')
      | 'A' <= c && c <= 'F' = Just (10 + digitToInt c - digitToInt 'A')
      | otherwise = Nothing
    go [] = []
    go ('%':a:b:xs) =
      case (hexVal a, hexVal b) of
        (Just hi, Just lo) -> toEnum (hi * 16 + lo) : go xs
        _ -> '%' : a : b : go xs
    go (x:xs) = x : go xs

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
