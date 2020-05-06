{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Adapter.HTTP.Server where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Servant

import Adapter.HTTP.Api
import Types
import Adapter.ToGD.ToGameDisplay (toGameDisplay)


import qualified GE.CreateGame as CG
import GE.Types
import qualified GE.GameRunner as GR
import           GE.UndoRedo


server
  :: Pool Connection
  -> MVar Game
  -> Game
  -> MVar (UndoRedo Game)
  -> Server API
server conns gameMVar initGame urMVar =
    getGame :<|>
    moveReq :<|>
    setDirection UP    :<|>
    setDirection RIGHT :<|>
    setDirection DOWN  :<|>
    setDirection LEFT  :<|>
    resetGame          :<|>
    undoGame           :<|>
    redoGame           :<|>
    gameAutoPlay
  where 
    gameConfig = gGameConfig initGame

    getGame :: Handler GameDisplay
    getGame = do
      game <- liftIO $ readMVar gameMVar
      return (toGameDisplay game)

    moveReq :: Handler GameDisplay
    moveReq = do
      gs' <-  liftIO $ fmap (GR.gameStep Move) 
                     $ takeMVar gameMVar
      liftIO $ putMVar gameMVar gs'
      liftIO $ modifyMVar_ urMVar (return . addToUR gs')
      return (toGameDisplay gs')
    
    setDirection :: Direction -> Handler GameDisplay
    setDirection d = do
      gs' <-  liftIO $ fmap (GR.gameStep (SetDirection d)) 
                     $ takeMVar gameMVar
      liftIO $ putMVar gameMVar gs'
      liftIO $ modifyMVar_ urMVar (return . addToUR gs')
      return (toGameDisplay gs')
    
    resetGame :: Handler GameDisplay
    resetGame = do
      liftIO $ takeMVar gameMVar
      liftIO $ putMVar gameMVar initGame
      liftIO $ modifyMVar_ urMVar (return . resetUndoRedo)
      return (toGameDisplay initGame)
    
    undoGame :: Handler GameDisplay
    undoGame = do
      ur <- liftIO $ readMVar urMVar
      currGameWorld <- liftIO $ readMVar gameMVar
      let (newGameWorld, newUR) = undoWithDef ur currGameWorld
      liftIO $ modifyMVar_ urMVar (return . const newUR)
      liftIO $ modifyMVar_ gameMVar (return . const newGameWorld)
      return (toGameDisplay newGameWorld)

    redoGame :: Handler GameDisplay
    redoGame = do
      ur <- liftIO $ readMVar urMVar
      currGameWorld <- liftIO $ readMVar gameMVar
      let (newGameWorld, newUR) = redoWithDef ur currGameWorld
      liftIO $ modifyMVar_ urMVar (return . const newUR)
      liftIO $ modifyMVar_ gameMVar (return . const newGameWorld)
      return (toGameDisplay newGameWorld)
    
    gameAutoPlay :: Handler GameDisplay
    gameAutoPlay = do
      gs' <-  liftIO . fmap GR.gameAutoPlay 
                     $ takeMVar gameMVar
      liftIO $ putMVar gameMVar gs'
      liftIO $ modifyMVar_ urMVar (return . addToUR gs')
      return (toGameDisplay gs')
