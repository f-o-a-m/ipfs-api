module V0.PinAPISpec where

import           Network.IPFS.API.V0.Types
import           Network.IPFS.Client
import           Test.Hspec
import           Utils

spec :: Spec
spec = describe "the v0 Pin API" . withClient $ do
  it "can add pins" $ \c -> pending
  it "can list pins" $ \c -> do
    res <- try c (v0PinLs Nothing Nothing)
    True `shouldBe` True -- test we decoded correctly/try didnt throw

  it "can update pins" $ \c -> pending
  it "can remove pins" $ \c -> pending
  it "can verify pins" $ \c -> pending
