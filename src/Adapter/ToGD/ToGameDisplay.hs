
module Adapter.ToGD.ToGameDisplay where


import qualified Data.HashMap.Strict as M
import Data.List

import GE.CreateGame
import GE.Types

import Types

toGameDisplay :: Game -> GameDisplay
toGameDisplay g =
  GameDisplay 0 0 $ (fmap coordinateToCD) <$> (gcGrid gc)
  
  where
    gc     =  gGameConfig g
    r      =  gRobot g
    rVisPM =  rVisPortMeta r
    cPorts =  vpmCPorts rVisPM
    oPorts =  vpmOPorts rVisPM

    -- Visited map combined ports from open and closed ports
    visCDMap = fmap (pMToCCode False) (cPorts <> oPorts)
    -- have to insert the robo too in the map
    cDMapWithRobo = M.insert (rPos r) (pMToCCode True (rPortMeta r)) $ visCDMap <> blkCDMap
    -- CellDetail Map for blocked / Obstacles Points
    blkCDMap = M.fromList $ zip (gcObstacles gc) (repeat Obs)
    -- Combined Map for cells containing visited mark, robot or Obstacles
    cDMapContainingSomething = blkCDMap <> cDMapWithRobo
    coordinateToCD coord@(Coordinate x y) = 
      case M.lookup coord cDMapContainingSomething of
        Just cc -> CellDetails x y cc
        Nothing -> CellDetails x y Blank


pMToCCode :: Bool -> PortMeta -> CellCode
pMToCCode True  ( PortMeta True True True True _ _ )     = Robo 
pMToCCode True  ( PortMeta True True True False _ _ )    = RoboL 
pMToCCode True  ( PortMeta True True False True _ _ )    = RoboD 
pMToCCode True  ( PortMeta True True False False _ _ )   = RoboDL 
pMToCCode True  ( PortMeta True False True True _ _ )    = RoboR 
pMToCCode True  ( PortMeta True False True False _ _ )   = RoboRL 
pMToCCode True  ( PortMeta True False False True _ _ )   = RoboRD 
pMToCCode True  ( PortMeta True False False False _ _ )  = RoboRDL 
pMToCCode True  ( PortMeta False True True True _ _ )    = RoboU 
pMToCCode True  ( PortMeta False True True False _ _ )   = RoboUL 
pMToCCode True  ( PortMeta False True False True _ _ )   = RoboUD 
pMToCCode True  ( PortMeta False True False False _ _ )  = RoboUDL 
pMToCCode True  ( PortMeta False False True True _ _ )   = RoboUR 
pMToCCode True  ( PortMeta False False True False _ _ )  = RoboURL 
pMToCCode True  ( PortMeta False False False True _ _ )  = RoboURD 
pMToCCode True  ( PortMeta False False False False _ _ ) = RoboURDL
pMToCCode False ( PortMeta True True True True _ _ )     = Vis 
pMToCCode False ( PortMeta True True True False _ _ )    = VisL 
pMToCCode False ( PortMeta True True False True _ _ )    = VisD 
pMToCCode False ( PortMeta True True False False _ _ )   = VisDL 
pMToCCode False ( PortMeta True False True True _ _ )    = VisR 
pMToCCode False ( PortMeta True False True False _ _ )   = VisRL 
pMToCCode False ( PortMeta True False False True _ _ )   = VisRD 
pMToCCode False ( PortMeta True False False False _ _ )  = VisRDL 
pMToCCode False ( PortMeta False True True True _ _ )    = VisU 
pMToCCode False ( PortMeta False True True False _ _ )   = VisUL 
pMToCCode False ( PortMeta False True False True _ _ )   = VisUD 
pMToCCode False ( PortMeta False True False False _ _ )  = VisUDL 
pMToCCode False ( PortMeta False False True True _ _ )   = VisUR 
pMToCCode False ( PortMeta False False True False _ _ )  = VisURL 
pMToCCode False ( PortMeta False False False True _ _ )  = VisURD 
pMToCCode False ( PortMeta False False False False _ _ ) = VisURDL
pMToCCode _     _                                   = Obs
