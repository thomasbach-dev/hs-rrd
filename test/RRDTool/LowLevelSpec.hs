module RRDTool.LowLevelSpec
  ( spec
  ) where

import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
import Test.Syd           (Spec, describe, it, runIO, shouldBe)

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

  describe "rrd_clear_error" $ do
    result <- runIO rrd_clear_error
    it "just runs" $ do
       result `shouldBe` ()

  describe "rrd_get_error" $ do
    result <- runIO rrd_get_error
    it "returns an empty string" $ do
      result `shouldBe` ""

  describe "rrd_test_error" $ do
    it "returns a 0" $ do
      rrd_test_error `shouldBe` 0

  describe "bound threads" $ do
    it "are supported" $ do
      rtsSupportsBoundThreads
    result <- runIO isCurrentThreadBound
    it "we are currently running in one" $ do
      result

  describe "rrd_new_context" $ do
    result <- runIO $ do
      ctx <- rrd_new_context
      (,) <$> rrdContextLibErrstr ctx <*> rrdContextRRDError ctx
    it "returns an empty RRDContext" $
      result `shouldBe` ("", "")


  describe "rrd_get_context" $ do
    result <- runIO $ do
      ctx <- rrd_get_context
      (,) <$> rrdContextLibErrstr ctx <*> rrdContextRRDError ctx
    it "returns an empty RRDContext" $ do
      result `shouldBe` ("", "")

