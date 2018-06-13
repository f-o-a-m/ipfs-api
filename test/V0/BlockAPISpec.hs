module V0.BlockAPISpec where

import           Test.Hspec
import           Utils

spec :: Spec
spec = describe "the v0 Block API" . withClient $ do
  it "can get raw blocks" $ \c -> pending
  it "can put raw blocks" $ \c -> pending
  it "can stat raw blocks" $ \c -> pending
  it "can delete raw blocks" $ \c -> pending
