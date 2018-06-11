module Network.IPFS.Client where

import           Data.ByteString           (ByteString)
import           Data.Proxy
import           Network.IPFS.API.V0
import           Network.IPFS.API.V0.Types
import           Servant.API               ((:<|>) (..))
import           Servant.Client

v0GetNode :<|> v0GetObject :<|> v0PostObject = client apiV0
