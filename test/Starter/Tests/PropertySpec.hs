module Starter.Tests.PropertySpec
  ( spec,
  )
where

import Control.Exception (Exception, throwIO)
import Minithesis qualified as Minithesis
import Minithesis.Property qualified as MP
import Starter.Prelude
import Test.Syd

-- | Property: addition is commutative for Int32.
spec :: Spec
spec =
  describe "Property" $ do
    it "addition commutative" $
      runMinithesis additionCommutative

newtype AdditionFailure = AdditionFailure (Int32, Int32)
  deriving (Show)

instance Exception AdditionFailure

additionCommutative :: MP.Property
additionCommutative =
  MP.withTests 200 $
    \tc -> do
      xVal <- Minithesis.any tc (Minithesis.integers (-10000) 10000)
      yVal <- Minithesis.any tc (Minithesis.integers (-10000) 10000)
      let x = fromIntegral xVal :: Int32
          y = fromIntegral yVal :: Int32
      unless (x + y == y + x) $
        throwIO (AdditionFailure (x, y))

runMinithesis :: MP.Property -> IO ()
runMinithesis property = do
  let base = MP.applyPropertyOptions Minithesis.defaultRunOptions property
  opts <- Minithesis.resolveRunOptions base
  MP.runProperty opts property
