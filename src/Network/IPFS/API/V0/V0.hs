module Network.IPFS.API.V0.V0 where

import           Data.ByteString.Lazy       (ByteString)
import           Servant.API
import           Servant.MultipartFormData

import           Network.IPFS.API.V0.Quirks
import           Network.IPFS.API.V0.Types

type RootApi = GetNodeID :<|> CatObject :<|> PostAddObjects

-- GET /api/v0/id
type GetNodeID =
  "id" :> IPFSArgOpt "ID of node to get info for" String
       :> Post '[JSON] IPFSNodeInfo

-- POST /api/v0/cat
type CatObject =
  "cat" :> IPFSArg "ipfs path of object" IPFSPath
        :> Post '[PlainTextBinary] ByteString

-- POST /api/v0/add
type PostAddObjects =
  "add" :> MultipartFormDataReqBody [AddFile]
        :> Post '[SequentialJSON] [AddObjectResponse]
