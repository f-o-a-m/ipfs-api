-- totally not inspired by the `telegram-api` package
module Servant.MultipartFormData
  ( ToMultipartFormData (..)
  , MultipartFormDataReqBody
  ) where

import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Data.Binary.Builder                   (toLazyByteString)
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BL
import           Data.IORef                            (modifyIORef', newIORef,
                                                        readIORef)
import           Data.Proxy                            (Proxy (..))
import           Network.HTTP.Client                   (RequestBody (..))
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Media.MediaType
import           Servant.API
import           Servant.Client
import           Servant.Client.Core                   hiding (RequestBody (..))
import qualified Servant.Client.Core                   as Servant 

-- | A type that can be converted to a multipart/form-data value.
class ToMultipartFormData a where
  -- | Convert a Haskell value to a multipart/form-data-friendly intermediate type.
  toMultipartFormData :: a -> [Part]

-- | Extract the request body as a value of type @a@.
data MultipartFormDataReqBody a

instance (MonadIO m, RunClient m, MimeUnrender ct a, ToMultipartFormData b) =>
  HasClient m (MultipartFormDataReqBody b :> Post (ct ': cts) a) where
    type Client m (MultipartFormDataReqBody b :> Post (ct ': cts) a) = b -> m a
    clientWithRoute pm _ req mpdata = do
      boundary <- liftIO webkitBoundary
      reqBody <- liftIO $ renderParts boundary (toMultipartFormData mpdata)
      body <- liftIO $ renderRequestBody reqBody
      let mediaType = "multipart" // "form-data" /: ("boundary", boundary)
          req' = req { requestBody = Just (Servant.RequestBodyLBS body, mediaType) }
      clientWithRoute pm (Proxy :: Proxy (Post (ct ': cts) a)) req'

      where renderRequestBody = \case
              RequestBodyLBS lbs -> return lbs
              RequestBodyBS  sbs -> return (BL.fromStrict sbs)
              RequestBodyBuilder _ builder -> return (toLazyByteString builder)
              RequestBodyStream _ gp -> collectPopper gp
              RequestBodyStreamChunked gp -> collectPopper gp
              RequestBodyIO io -> renderRequestBody =<< io

            collectPopper gp = do
              acc <- newIORef []
              gp (popInto acc)
              BL.fromChunks . reverse <$> readIORef acc

            popInto ref popper = do
              popped <- popper
              if BS.null popped
                then return ()
                else do
                  modifyIORef' ref (popped:)
                  popInto ref popper


