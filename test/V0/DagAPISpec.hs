module V0.DagAPISpec where

import qualified Data.IPLD.CID             as IPLD
import           Data.String.Conversions   (cs)
import           Network.IPFS.API.V0.Types
import           Network.IPFS.Client
import           Test.Hspec
import           Test.Hspec.Expectations
import           Utils

spec :: Spec
spec = describe "the v0 Dag API" . withClient $ do

  it "can put dags" $ \c -> do
    (CIDResponse cid) <- try c $ v0DagPut (Just DagJSON) [AddFile "test.json" testJSONFileContents]
    let cidString = cs . IPLD.cidToText $ cid
    cidString `shouldBe` testJSONHash

  it "can get dags" $ \c -> do
    getRes <- try c $ v0DagGet testJSONHash (Just DagJSON)
    getRes `shouldBe` testJSONFileContents
