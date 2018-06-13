module Network.IPFS.Client where

import           Data.ByteString.Lazy      (ByteString)
import           Data.Proxy
import           Network.IPFS.API.V0
import           Network.IPFS.API.V0.Types
import           Servant.API               ((:<|>) (..))
import           Servant.Client

v0GetNode        :: Maybe String -> ClientM IPFSNodeInfo
v0CatObject      :: Multihash -> ClientM ByteString
v0PostAddObjects :: [AddFile] -> ClientM [AddObjectResponse]
v0BlockGet       :: Multihash -> ClientM ByteString
v0BlockPut       :: BlockPutData -> ClientM BlockStatResponse
v0BlockStat      :: Multihash -> ClientM BlockStatResponse
v0BlockRm        :: Multihash -> ClientM BlockRmResponse
v0PinAdd         :: String -> Bool -> Bool -> ClientM PinModResponse
v0PinLs          :: Maybe [Char] -> Maybe PinListType -> ClientM PinLsResponse
v0PinRm          :: String -> Bool -> ClientM PinModResponse
v0PinUpdate      :: String -> String -> Bool -> ClientM String
v0PinVerify      :: ClientM ()

(v0GetNode :<|> v0CatObject :<|> v0PostAddObjects)
  :<|> (v0BlockGet :<|> v0BlockPut :<|> v0BlockStat :<|> v0BlockRm)
  :<|> (v0PinAdd :<|> v0PinLs :<|> v0PinRm :<|> v0PinUpdate :<|> v0PinVerify) = client apiV0
