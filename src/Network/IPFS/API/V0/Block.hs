module Network.IPFS.API.V0.Block where

import           Data.ByteString.Lazy       (ByteString)
import           Servant.API
import           Servant.MultipartFormData

import           Network.IPFS.API.V0.Quirks
import           Network.IPFS.API.V0.Types

type BlockApi = (BlockGet :<|> BlockPut :<|> BlockStat :<|> BlockRm)

type BlockGet =
  "block" :> "get"
          :> IPFSArg "block-multihash" Multihash
          :> Post '[PlainTextBinary] ByteString

type BlockPut =
  "block" :> "put"
          :> MultipartFormDataReqBody PutData
          :> Post '[JSON] BlockStatResponse

type BlockStat =
  "block" :> "stat"
          :> IPFSArg "block-multihash" Multihash
          :> Post '[JSON] BlockStatResponse

type BlockRm =
  "block" :> "rm"
          :> IPFSArg "block-multihash" Multihash
          :> Post '[JSON] BlockRmResponse
