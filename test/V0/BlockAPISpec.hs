module V0.BlockAPISpec where

import           Network.IPFS.Client
import           Test.Hspec
import           Utils

spec :: Spec
spec = describe "the v0 Block API" . withClient $ do
  it "can get raw blocks" $ \c -> do
    res <- try c (v0BlockGet testfileTxtHash)
    True `shouldBe` True -- test we decoded correctly/try didnt throw

  it "can put raw blocks" $ \c -> pending

  it "can stat raw blocks" $ \c -> do
    res <- try c (v0BlockStat testfileTxtHash)
    True `shouldBe` True -- test we decoded correctly/try didnt throw

  it "can delete raw blocks" $ \c -> do
    res <- try c (v0BlockRm testfileTxtHash)
    True `shouldBe` True -- test we decoded correctly/try didnt throw
