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
import qualified GE.GamePlay as GP
import           GE.UndoRedo


server
  :: Pool Connection
  -> MVar GameWorld
  -> GameWorld 
  -> MVar (UndoRedo GameWorld)
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
    redoGame           
  where   
    getGame :: Handler GameDisplay
    getGame = do
      gameWorld <- liftIO $ readMVar gameMVar
      return (toGameDisplay gameWorld)

    moveReq :: Handler GameDisplay
    moveReq = do
      gs' <-  liftIO $ fmap GP.runRobo $ takeMVar gameMVar
      liftIO $ putMVar gameMVar gs'
      liftIO $ modifyMVar_ urMVar (return . addToUR gs')
      return (toGameDisplay gs')
    
    setDirection :: Direction -> Handler GameDisplay
    setDirection d = do
      gs <- liftIO $ takeMVar gameMVar
      let gs' = GP.setRoboDir d gs
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


defGame2 = toGameDisplay $ CG.makeSampleGameWorld 15 15 

defGame = GameDisplay 3 3 sampleGD

sampleGD = makeGrid 30 15 
                         
makeGrid :: Int -> Int -> [[CellDetails]]
makeGrid x y = zipWith f (makeRow x <$> [1..y]) (gh x cc)
  where
    makeRow x y = [CellDetails a y | a <- [1..x]] 
    cc = (fmap toEnum $ cycle [0..33])
    f :: [CellCode -> CellDetails] -> [CellCode] -> [CellDetails]
    f = zipWith ($)
    gh b [] = []
    gh b xs = take b xs  : gh b (drop b xs) 