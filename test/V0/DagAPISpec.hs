module V0.DagAPISpec where

import           Network.IPFS.API.V0.Types
import           Network.IPFS.Client
import           Test.Hspec
import           Test.Hspec.Expectations
import           Utils

spec :: Spec
spec = describe "the v0 Dag API" . withClient $ do
  it "can put and get" $ \c -> do
    (CIDResponse cid) <- try c $ v0DagPut (Just DagJSON) [AddFile "test.json" testJSONFileContents]
    getRes <- try c $ v0DagGet cid (Just DagJSON)
    getRes `shouldBe` testJSONFileContents
