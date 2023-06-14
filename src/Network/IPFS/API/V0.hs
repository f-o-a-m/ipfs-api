module Network.IPFS.API.V0
    ( ApiV0
    , apiV0
    , module Block
    , module Pin
    , module Quirks
    , module Types
    , module V0
    ) where

import           Data.Proxy
import           Network.IPFS.API.V0.Block  as Block
import           Network.IPFS.API.V0.Dag    as Dag
import           Network.IPFS.API.V0.Pin    as Pin
import           Network.IPFS.API.V0.Quirks as Quirks
import           Network.IPFS.API.V0.Types  as Types
import           Network.IPFS.API.V0.V0     as V0
import           Servant.API

type ApiV0 = "api" :> "v0" :>
  (    V0.RootApi
  :<|> Block.BlockApi
  :<|> Pin.PinApi
  :<|> Dag.DagApi
  )

apiV0 :: Proxy ApiV0
apiV0 = Proxy

