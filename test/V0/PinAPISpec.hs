module V0.PinAPISpec where

import           Network.IPFS.API.V0.Types
import           Network.IPFS.Client
import           Test.Hspec
import           Utils

spec :: Spec
spec = describe "the v0 Pin API" . withClient $ do
  it "can add pins" $ \c -> do
    res <- try c (v0PinAdd testfileTxtHash False False)
    res `shouldBe` (PinModResponse [testfileTxtHash])

  it "can list pins" $ \c -> do
    res <- try c (v0PinLs Nothing Nothing)
    True `shouldBe` True -- test we decoded correctly/try didnt throw

  it "can update pins" $ \c -> pending

  it "can remove pins" $ \c -> do
    res <- try c (v0PinRm testfileTxtHash False)
    res `shouldBe` (PinModResponse [testfileTxtHash])

  it "can verify pins" $ \c -> do
    res <- try c v0PinVerify
    True `shouldBe` True -- test we decoded correctly/try didnt throw
