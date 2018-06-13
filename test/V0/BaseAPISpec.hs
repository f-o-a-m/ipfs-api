module V0.BaseAPISpec where

import qualified Data.ByteString.Lazy      as BL
import           Network.IPFS.API.V0.Types
import           Network.IPFS.Client
import           Test.Hspec
import           Test.Hspec.Expectations
import           Utils

spec :: Spec
spec = describe "the v0 Base API" . withClient $ do
  it "can put objects" $ \c -> do
    res <- try c (v0PostAddObjects [AddFile "testfile.txt" testfileTxtContents])
    res `shouldSatisfy` (not . null)
    let ret = head res
    arName ret `shouldBe` "testfile.txt"
    arHash ret `shouldBe` testfileTxtHash

  it "can cat objects" $ \c -> do
    res <- try c (v0CatObject testfileTxtHash)
    res `shouldBe` testfileTxtContents

  it "can get node info" $ \c -> do
    res <- try c $ v0GetNode Nothing
    True `shouldBe` True -- just seeing that this doesnt get a Left

