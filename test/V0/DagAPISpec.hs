module V0.DagAPISpec where

import           Network.IPFS.API.V0.Types
import           Network.IPFS.Client
import           Test.Hspec
import           Test.Hspec.Expectations
import           Utils

spec :: Spec
spec = describe "the v0 Dag API" . withClient $ do
  it "can put dags" $ \c -> do
    res <- try c $ v0DagPut (Just DagJSON) [AddFile "test.json" testJSONFileContents]
    res `shouldBe` CIDResponse testJSONHash

  it "can get dags" $ \c -> do
    res <- try c $ v0DagGet testJSONHash (Just DagJSON)
    res `shouldBe` testJSONFileContents
