module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Starter.Prelude
import Starter.Server (appOpenApi)

main :: IO ()
main = BL.putStrLn (encode appOpenApi)
