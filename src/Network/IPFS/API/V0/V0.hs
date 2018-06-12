module Network.IPFS.API.V0.V0 where

import           Data.ByteString            (ByteString)
import           Servant.API
import           Servant.MultipartFormData

import           Network.IPFS.API.V0.Quirks
import           Network.IPFS.API.V0.Types

-- This is a convenience synonym around the fact that IPFS's RPC API uses the argument
-- "arg" _ALL_ _OVER_ _THE_ _PLACE_. In some cases, it even uses it more than once in the
-- same endpoint! The way you'd use this, for example, in the '/api/v0/files/cp' endpoint, is
-- ApiV0 (IPFSReqArg "from" String :> IPFSReqArg "to" String :> Get '[JSON] FilesCPResponse)
type IPFSArg d a = QueryParam' '[Required, Strict, Description d] "arg" a

-- Ditto, but for when "arg" is optional
type IPFSArgOpt d a = QueryParam' '[Optional, Strict, Description d] "arg" a

-- TODO: This should be a multihash/multipath/whatever IPFS actually uses
type IPFSPath = String

-- GET /api/v0/object
type GetObject =
  "get" :> IPFSArg "ipfs path of object" IPFSPath
        :> Get '[PlainTextBinary] ByteString

-- GET /api/v0/id
type GetNodeID =
  "id" :> IPFSArgOpt "ID of node to get info for" String
       :> Get '[JSON] IPFSNodeInfo

-- POST /api/v0/add
type PostAddObjects =
  "add" :> MultipartFormDataReqBody [AddFile]
        :> Post '[SequentialJSON] [AddObjectResponse]
