module Network.IPFS.API.V0.Types where

import           Control.Applicative                   ((<|>))
import           Data.Aeson
import           Data.ByteString.Lazy                  (ByteString)
import           Data.Semigroup                        (Semigroup (..))
import qualified Data.Text                             as Text
import           Network.HTTP.Client                   (RequestBody (RequestBodyLBS))
import           Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import           Network.IPFS.API.V0.Quirks
import           Servant.API
import           Servant.MultipartFormData

import           Debug.Trace


-- This is a convenience synonym around the fact that IPFS's RPC API uses the argument
-- "arg" _ALL_ _OVER_ _THE_ _PLACE_. In some cases, it even uses it more than once in the
-- same endpoint! The way you'd use this, for example, in the '/api/v0/files/cp' endpoint, is
-- ApiV0 (IPFSReqArg "from" String :> IPFSReqArg "to" String :> Get '[JSON] FilesCPResponse)
type IPFSArg d a = QueryParam' '[Required, Strict, Description d] "arg" a

-- Ditto, but for when "arg" is optional
type IPFSArgOpt d a = QueryParam' '[Optional, Strict, Description d] "arg" a

-- TODO: This should be a multihash/multipath/whatever IPFS actually uses
type IPFSPath = String

-- Some temporary type synonyms until we implement multihash/addr/etc. correctly
type Multihash = String
type Multiaddr = String

--------------------------------------------------------------
-- data for IPFS Node, as received from /api/v0/id with no
-- `format` arg specified
data IPFSNodeInfo = IPFSNodeInfo { iniID              :: String
                                 , iniPubKey          :: String
                                 , iniAddresses       :: [Multiaddr]
                                 , iniAgentVersion    :: String
                                 , iniProtocolVersion :: String
                                 }
                                 deriving (Eq, Ord, Read, Show)

instance FromJSON IPFSNodeInfo where
  parseJSON = withObject "IPFSNodeInfo" $ \o ->
    IPFSNodeInfo <$> o .: "ID"
                 <*> o .: "PublicKey"
                 <*> o .: "Addresses"
                 <*> o .: "AgentVersion"
                 <*> o .: "ProtocolVersion"

--------------------------------------------------------------
-- data for the response to /api/v0/add
-- assuming no quiet/quieter/etc. specified
data AddFile = AddFile { afName :: String
                       , afData :: ByteString
                       }
                       deriving (Eq, Ord, Read, Show)

instance ToMultipartFormData [AddFile] where
  toMultipartFormData files = partify <$> zip [1..] files
    where partify (idx, file) = partFileRequestBody (fileIndex idx) (afName file) (RequestBodyLBS $ afData file)
          fileIndex idx = "file" <> Text.pack (show idx)

--------------------------------------------------------------
-- data for a single response from /api/v0/add
-- assuming no quiet/quieter/etc. specified
data AddObjectResponse = AddObjectResponse { arName :: String
                                           , arHash :: Multihash
                                           , arSize :: Integer
                                           }
                                           deriving (Eq, Ord, Read, Show)

instance FromJSON AddObjectResponse where
  parseJSON c = trace (show c) (flip $ withObject "AddObjectResponse") c $ \o ->
    AddObjectResponse <$> o .: "Name"
                      <*> o .: "Hash"
                      <*> (o .: "Size" <|> (read <$> o .: "Size")) -- Size is Strung sometimes ಠ_______________ಠ

--------------------------------------------------------------
-- multipart form data for /api/v0/block/put
newtype BlockPutData = BlockPutData ByteString
  deriving (Eq, Ord, Read, Show)

instance ToMultipartFormData BlockPutData where
  toMultipartFormData (BlockPutData d) = [partFileRequestBody "arg" "arg" (RequestBodyLBS d)]

--------------------------------------------------------------
-- response for /api/v0/block/put and .../block/stat
data BlockStatResponse = BlockStatResponse { bsrHash :: Multihash
                                           , bsrSize :: Integer
                                           }
                                           deriving (Eq, Ord, Read, Show)

instance FromJSON BlockStatResponse where
  parseJSON = withObject "BlockStatResponse" $ \o ->
    BlockStatResponse <$> o .: "Key"
                      <*> o .: "Size"

--------------------------------------------------------------
-- response for /api/v0/block/rm
data BlockRmResponse = BlockRmResponse { brrHash  :: Multihash
                                       , brrError :: Maybe String
                                       }
                                       deriving (Eq, Ord, Read, Show)

instance FromJSON BlockRmResponse where
  parseJSON = withObject "BlockRmResponse" $ \o ->
    BlockRmResponse <$> o .:  "Hash"
                    <*> o .:? "Error"

--------------------------------------------------------------
-- type representing valid values to the type param in /api/v0/pin/ls
data PinListType = PinListDirect
                 | PinListIndirect
                 | PinListRecursive
                 | PinListAll
                 deriving (Eq, Ord, Read, Show)

instance ToHttpApiData PinListType where
  toQueryParam PinListDirect    = "direct"
  toQueryParam PinListIndirect  = "indirect"
  toQueryParam PinListRecursive = "recursive"
  toQueryParam PinListAll       = "all"

instance FromHttpApiData PinListType where
  parseQueryParam "direct"    = Right PinListDirect
  parseQueryParam "indirect"  = Right PinListIndirect
  parseQueryParam "recursive" = Right PinListRecursive
  parseQueryParam "all"       = Right PinListAll
  parseQueryParam _           = Left "Expected one of: direct, indirect, recusive, all"
