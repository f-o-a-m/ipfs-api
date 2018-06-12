module Network.IPFS.API.V0.V0 where

import           Data.ByteString.Lazy       (ByteString)
import           Servant.API
import           Servant.MultipartFormData

import           Network.IPFS.API.V0.Quirks
import           Network.IPFS.API.V0.Types

type RootApi = GetNodeID :<|> GetObject :<|> PostAddObjects

-- GET /api/v0/id
type GetNodeID =
  "id" :> IPFSArgOpt "ID of node to get info for" String
       :> Get '[JSON] IPFSNodeInfo

-- GET /api/v0/object
type GetObject =
  "get" :> IPFSArg "ipfs path of object" IPFSPath
        :> Get '[PlainTextBinary] ByteString

-- POST /api/v0/add
type PostAddObjects =
  "add" :> MultipartFormDataReqBody [AddFile]
        :> Post '[SequentialJSON] [AddObjectResponse]
