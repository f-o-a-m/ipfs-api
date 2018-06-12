module Network.IPFS.API.V0.Types where

import           Data.Aeson
import           Data.ByteString.Lazy                  (ByteString)
import           Data.Semigroup                        (Semigroup (..))
import qualified Data.Text                             as Text
import           Network.HTTP.Client                   (RequestBody (RequestBodyLBS))
import           Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import           Servant.MultipartFormData

import           Network.IPFS.API.V0.Quirks

--------------------------------------------------------------
-- data for IPFS Node, as received from /api/v0/id with no
-- `format` arg specified
data IPFSNodeInfo = IPFSNodeInfo { iniID              :: String
                                 , iniPubKey          :: String
                                 , iniAddresses       :: [String] -- Should be [Multiaddr]
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
                                           , arHash :: String -- Should be Multihash
                                           , arSize :: Integer
                                           }
                                           deriving (Eq, Ord, Read, Show)

instance FromJSON AddObjectResponse where
  parseJSON = withObject "AddObjectResponse" $ \o ->
    AddObjectResponse <$> o .: "Name"
                      <*> o .: "Hash"
                      <*> (read <$> o .: "Size") -- Size is Strung sometimes ಠ_______________ಠ
