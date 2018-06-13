module Network.IPFS.API.V0.Pin where

import           Data.ByteString.Lazy       (ByteString)
import           Servant.API
import           Servant.MultipartFormData

import           Network.IPFS.API.V0.Quirks
import           Network.IPFS.API.V0.Types

type PinApi =  PinAdd :<|> PinLs :<|> PinRm :<|> PinUpdate :<|> PinVerify

type PinAdd =
  "pin" :> "add"
        :> IPFSArg "path" String
        :> QueryFlag "recursive"
        :> QueryFlag "progress"
        :> Get '[JSON] PinModResponse

type PinLs =
  "pin" :> "ls"
        :> IPFSArgOpt "path" String
        :> QueryParam "type" PinListType
        :> Get '[JSON] PinLsResponse

type PinRm =
  "pin" :> "rm"
        :> IPFSArg "unpin" String
        :> QueryFlag "recursive"
        :> Get '[JSON] PinModResponse

type PinUpdate =
  "pin" :> "update"
        :> IPFSArg "old" String
        :> IPFSArg "new" String
        :> QueryFlag "unpin"
        :> Get '[JSON] String -- PinUpdateResponse

type PinVerify =
  "pin" :> "verify"
        :> Get '[RespondsWithNothing] () -- it response with nothing??
