module Network.IPFS.Client where

import           Data.ByteString.Lazy      (ByteString)
import           Data.Proxy
import           Network.IPFS.API.V0
import           Network.IPFS.API.V0.Types
import           Servant.API               ((:<|>) (..))
import           Servant.Client

v0GetNode        :: Maybe String -> ClientM IPFSNodeInfo
v0GetObject      :: Multihash -> ClientM ByteString
v0PostAddObjects :: [AddFile] -> ClientM [AddObjectResponse]
v0BlockGet       :: Multihash -> ClientM ByteString
v0BlockPut       :: BlockPutData -> ClientM BlockStatResponse
v0BlockStat      :: Multihash -> ClientM BlockStatResponse
v0BlockRm        :: Multihash -> ClientM BlockRmResponse

v0GetNode :<|> v0GetObject :<|> v0PostAddObjects :<|> v0BlockGet :<|> v0BlockPut :<|> v0BlockStat :<|> v0BlockRm = client apiV0
