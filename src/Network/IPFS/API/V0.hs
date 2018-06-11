module Network.IPFS.API.V0
    ( ApiV0
    , apiV0
    , module Types
    , module V0
    ) where

import           Data.Proxy
import           Network.IPFS.API.V0.Types as Types
import           Network.IPFS.API.V0.V0    as V0
import           Servant.API

type ApiV0 = "api" :> "v0" :>
        (    V0.GetNodeID
        :<|> V0.GetObject
        :<|> V0.PostAddObjects
        )

apiV0 :: Proxy ApiV0
apiV0 = Proxy

