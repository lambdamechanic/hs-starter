module Starter.Env
  ( AppEnv (..),
  )
where

import Starter.Database.Connection (DbConfig)
import Starter.OAuth.Types (OAuthProfile)
import Starter.Prelude

-- | Global environment values shared across the application runtime.
data AppEnv = AppEnv
  { appPort :: Int,
    otelServiceName :: Text,
    otelCollectorEndpoint :: Maybe String,
    otelCollectorHeaders :: Maybe String,
    dbConfig :: DbConfig,
    authorizeLogin :: OAuthProfile -> IO Bool
  }
