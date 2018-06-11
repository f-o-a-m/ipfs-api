-- totally not inspired by the `telegram-api` package
module Servant.MultipartFormData
  ( ToMultipartFormData (..)
  , MultipartFormDataReqBody
  ) where
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Data.Proxy
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Media.MediaType
import           Servant.API
import           Servant.Client
import           Servant.Client.Core

-- | A type that can be converted to a multipart/form-data value.
class ToMultipartFormData a where
  -- | Convert a Haskell value to a multipart/form-data-friendly intermediate type.
  toMultipartFormData :: a -> [Part]

-- | Extract the request body as a value of type @a@.
data MultipartFormDataReqBody a

instance (MonadIO m, RunClient m, MimeUnrender ct a, ToMultipartFormData b) => HasClient m (MultipartFormDataReqBody b :> Post (ct ': cts) a) where
    type Client m (MultipartFormDataReqBody b :> Post (ct ': cts) a) = b -> m a
    clientWithRoute pm _ req mpdata = do
        boundary <- liftIO webkitBoundary
        body <- liftIO $ renderParts boundary (toMultipartFormData mpdata)
        let mediaType = "multipart" // "form-data" /: ("boundary", boundary)
            req' = req { requestBody = Just (RequestBodyLBS undefined, mediaType) }
        clientWithRoute pm (Proxy :: Proxy (Post (ct ': cts) a)) req'
