{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.HTTP.Api where

import Servant
import Data.Aeson
import GHC.Generics

import Types
import GE.Types

type API =
    "getGame" :> Get '[JSON] GameDisplay
   :<|>
    "moveReq" :> Get '[JSON] GameDisplay
   :<|>
    "turnUp" :> Get '[JSON] GameDisplay
   :<|>
    "turnRight" :> Get '[JSON] GameDisplay
   :<|>
    "turnDown" :> Get '[JSON] GameDisplay
   :<|>
    "turnLeft" :> Get '[JSON] GameDisplay
   :<|>
    "resetGame" :> Get '[JSON] GameDisplay
   :<|>
    "undoGame" :> Get '[JSON] GameDisplay
   :<|>
    "redoGame" :> Get '[JSON] GameDisplay

    -- "setDirection"
    --   :> ReqBody '[JSON] Direction
    --   :> Post '[JSON] GameDisplay
  

api :: Proxy (API :<|> Raw)
api = Proxy
