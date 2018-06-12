module Network.IPFS.Client where

import           Data.ByteString           (ByteString)
import           Data.Proxy
import           Network.IPFS.API.V0
import           Network.IPFS.API.V0.Types
import           Servant.API               ((:<|>) (..))
import           Servant.Client

v0GetNode :: Maybe String -> ClientM IPFSNodeInfo
v0GetObject :: String -> ClientM ByteString
v0PostAddObjects :: [AddFile] -> ClientM [AddObjectResponse]

v0GetNode :<|> v0GetObject :<|> v0PostAddObjects = client apiV0
