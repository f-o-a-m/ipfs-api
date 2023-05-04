module Network.IPFS.API.V0.Dag where

import           Data.ByteString.Lazy       (ByteString)
import           Servant.API
import           Servant.MultipartFormData
import           Network.IPFS.API.V0.Types
import           Network.IPFS.API.V0.Quirks

type DagApi = (GetDag :<|> PutDag)

type GetDag =
  "dag" :> "get"
        :> IPFSArg "dag-multihash" Multihash
        :> QueryParam "output-codec" DagDataEncoding
        :> Post '[PlainTextBinary] ByteString

type PutDag =
  "dag" :> "put"
        :> QueryParam "input-codec" DagDataEncoding
        :> MultipartFormDataReqBody [AddFile]
        :> Post '[JSON] CIDResponse