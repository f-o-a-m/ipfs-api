module Utils where

import           Control.Exception    (throwIO)
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Client  (defaultManagerSettings, newManager)
import           Network.HTTP.Types
import           Servant.Client
import           System.IO.Unsafe     (unsafePerformIO)
import           Test.Hspec

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv

globalEnv :: ClientEnv
globalEnv = unsafePerformIO $ do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" 5001 ""
  return $ ClientEnv manager baseUrl Nothing
{-# NOINLINE globalEnv #-}

withClient :: SpecWith ClientEnv -> Spec
withClient spec = before (return globalEnv) spec

-- the multihash of test/fixtures/testfile.txt as added via `ipfs add`
testfileTxtHash :: String
testfileTxtHash = "Qma4hjFTnCasJ8PVp3mZbZK5g2vGDT4LByLJ7m8ciyRFZP"

-- the exact contents of that file
testfileTxtContents :: BL.ByteString
testfileTxtContents = unsafePerformIO $ BL.readFile "test/fixtures/testfile"
{-# NOINLINE testfileTxtContents #-}
