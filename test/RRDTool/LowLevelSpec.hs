module RRDTool.LowLevelSpec
  ( spec
  ) where

import Test.Syd (Spec, describe, it, runIO, shouldBe)

import RRDTool.LowLevel

spec :: Spec
spec = do
  describe "rrd_strversion" $ do
    version <- runIO rrd_strversion
    it "returns the expected version of the library" $ do
      version `shouldBe` "1.7.2"
  describe "rrd_version" $ do
    it "returns the expected version of the library" $ do
      rrd_version `shouldBe` 1.7002
