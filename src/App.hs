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

import Data.Pool
import Database.PostgreSQL.Simple

run :: IO ()
run = do
  let port = 7000
  let connStr = ""
  pool <- initConnectionPool connStr
  initDB connStr
  game <- newMVar (CG.makeSampleGameWorld 20 15)
  let settings = setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  manager <- newManager defaultManagerSettings
  runSettings settings $ mkApp manager pool game
    
mkApp :: Manager -> Pool Connection -> MVar GameWorld -> Application
mkApp manager conns gameMVar = cors (const . Just $ corsPolicy) $
  (serve api $ (server conns gameMVar) :<|> forwardServer manager)
  where

    -- Need to explictly allow needed extra headers through CORS.
    corsPolicy = simpleCorsResourcePolicy
      { corsRequestHeaders = [ "content-type" ]
      }
    