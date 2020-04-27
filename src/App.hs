{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Data.Aeson
import           Control.Concurrent
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.ReverseProxy
import           Servant
import           System.IO

import           Network.Wai.Middleware.Cors

import           Adapter.HTTP.Api
import           Adapter.HTTP.Server
import           Adapter.HTTP.ProxyServer
import           Adapter.HTTP.PostgreSQL.UserData

import           GE.Types
import qualified GE.CreateGame as CG
import           GE.UndoRedo

import Data.Pool
import Database.PostgreSQL.Simple

run :: IO ()
run = do
  let port = 7000
  let connStr = ""
  pool <- initConnectionPool connStr
  initDB connStr
  let initGame = (CG.makeSampleGameWorld 10 10)
  game <- newMVar initGame
  undoRedo <- newMVar $ initUndoRedo 20
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  manager <- newManager defaultManagerSettings
  runSettings settings $ mkApp manager pool game initGame undoRedo
    
mkApp
  :: Manager
  -> Pool Connection
  -> MVar GameWorld             -- MVar Of Current Game World
  -> GameWorld                  -- Game World To Reset
  -> MVar (UndoRedo GameWorld)  -- MVar Of UndoRedo
  -> Application
mkApp manager conns gameMVar initGame initUR = cors (const . Just $ corsPolicy) $
  (serve api $ (server conns gameMVar initGame initUR) :<|> forwardServer manager)
  where

    -- Need to explictly allow needed extra headers through CORS.
    corsPolicy = simpleCorsResourcePolicy
      { corsRequestHeaders = [ "content-type" ]
      }
    