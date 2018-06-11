module Network.IPFS.API.V0.Types where

import           Data.Aeson
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BL
import           Data.Proxy
import           Data.Semigroup                        (Semigroup, (<>))
import           Data.String                           (IsString)
import qualified Data.Text                             as Text
import           Network.HTTP.Client                   (RequestBody (RequestBodyLBS))
import           Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import           Servant.API.ContentTypes              (Accept (..), JSON,
                                                        MimeRender (..),
                                                        MimeUnrender (..),
                                                        PlainText)
import           Servant.MultipartFormData

--------------------------------------------------------------
-- /api/v0/get returns Content-Type: text/plain
-- It still outputs raw binary.
-- :) :) :) :) :) :) :)
data PlainTextBinary

instance Accept PlainTextBinary where
    contentType _ = contentType (Proxy @PlainText)

instance MimeRender PlainTextBinary BL.ByteString where
    mimeRender _ = id

instance MimeUnrender PlainTextBinary BL.ByteString where
    mimeUnrender _ = Right

instance MimeRender PlainTextBinary BS.ByteString where
    mimeRender _ = BL.fromStrict

instance MimeUnrender PlainTextBinary BS.ByteString where
    mimeUnrender _ = Right . BL.toStrict

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
                       , afData :: BL.ByteString
                       }
                       deriving (Eq, Ord, Read, Show)

instance ToMultipartFormData [AddFile] where
    toMultipartFormData files = partify <$> (zip [1..] files)
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
                          <*> o .: "Size"

-- The response for multiple files submitted to /api/v0/add
-- isnt an array of objects, but just multiple JSON objects in a row.
-- i.e. {a: b} {a: c} instead of [{a: b}, {a: c}].
-- So we need a special MimeUnrender for it
data SequentialJSON

instance Accept SequentialJSON where
    contentType _ = contentType (Proxy @JSON)

instance FromJSON a => MimeUnrender SequentialJSON [a] where
    mimeUnrender = undefined

