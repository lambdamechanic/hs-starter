module Starter.Server
  ( app
  , apiProxy
  , Api
  , server
  , HealthStatus (..)
  ) where

import Starter.Prelude

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Hashable (Hashable)
import Servant

-- | API type definition for the Servant server.
type Api = Get '[JSON] HealthStatus

newtype HealthStatus = HealthStatus
  { status :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

instance ToJSON HealthStatus where
  toJSON HealthStatus {status} = object ["status" .= status]

apiProxy :: Proxy Api
apiProxy = Proxy

server :: Server Api
server = pure (HealthStatus "ok")

app :: env -> Application
app _env = serve apiProxy server
