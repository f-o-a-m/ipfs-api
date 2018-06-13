module V0.PinAPISpec where

import           Test.Hspec
import           Utils

spec :: Spec
spec = describe "the v0 Pin API" . withClient $ do
  it "can add pins" $ \c -> pending
  it "can list pins" $ \c -> pending
  it "can update pins" $ \c -> pending
  it "can remove pins" $ \c -> pending
  it "can verify pins" $ \c -> pending
