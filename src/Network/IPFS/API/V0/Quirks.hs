-- Servant support for IPFS HTTP-RPC quirks
module Network.IPFS.API.V0.Quirks where

import           Data.Aeson                   (FromJSON (..))
import qualified Data.Aeson.Parser            as AP
import           Data.Aeson.Types             (parseEither)
import qualified Data.Attoparsec.ByteString   as AT
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import           Data.List.NonEmpty           ((<|))
import           Data.Proxy                   (Proxy (..))
import           Data.Semigroup               ((<>))
import           Network.HTTP.Media.MediaType
import           Servant.API

--------------------------------------------------------------
-- /api/v0/get returns Content-Type: text/plain
-- It still outputs raw binary.
-- :) :) :) :) :) :) :)
data PlainTextBinary

instance Accept PlainTextBinary where
  contentTypes _ = ("text" // "plain") <| contentTypes (Proxy @PlainText)

instance MimeRender PlainTextBinary BL.ByteString where
  mimeRender _ = id

instance MimeUnrender PlainTextBinary BL.ByteString where
  mimeUnrender _ = Right

instance MimeRender PlainTextBinary BS.ByteString where
  mimeRender _ = BL.fromStrict

instance MimeUnrender PlainTextBinary BS.ByteString where
  mimeUnrender _ = Right . BL.toStrict

------------------------------------------------------------------------
-- The response for multiple files submitted to /api/v0/add
-- isnt an array of objects, but just multiple JSON objects in a row.
-- i.e. {a: b} {a: c} instead of [{a: b}, {a: c}].
-- So we need a special MimeUnrender for it
data SequentialJSON

instance Accept SequentialJSON where
  contentTypes _ = contentTypes (Proxy @JSON)

instance FromJSON a => MimeUnrender SequentialJSON [a] where
  mimeUnrender _ bl = -- we do the reverse after parsing so any non-json-parsing appear in the correct order
    mapM (parseEither parseJSON) =<< (reverse <$> go (BL.toStrict bl) [])

    where go bs acc | BS.null bs = Right acc
                    | bs == "\n" = Right acc -- v0PostAddObjects will \n-terminate
                    | otherwise  = let withResultOf = \case
                                         AT.Fail rem ctxts err -> Left ("SequentialJSON failed to parse in " ++ show ctxts ++ ": " ++ err ++ "; rem=" ++ show rem)
                                         AT.Partial c -> withResultOf (c "")
                                         AT.Done rem res -> go rem (res:acc)
                                    in withResultOf (AT.parse AP.json' bs)

-- For when we expect an endpoint to return absolutely nothing under normal cirucmstances
data RespondsWithNothing

instance Accept RespondsWithNothing where
  contentTypes _ = contentTypes (Proxy @JSON) <> contentTypes (Proxy @PlainText)

instance MimeUnrender RespondsWithNothing () where
  mimeUnrender _ _ = Right ()

instance MimeRender RespondsWithNothing a where
  mimeRender _ _ = ""
