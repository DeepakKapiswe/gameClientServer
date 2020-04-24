
module Adapter.ToGD.ToGameDisplay where

import qualified Data.Map as M
import Data.List

import GE.CreateGame
import GE.Types

import Types


toGameDisplay :: GameWorld -> GameDisplay
toGameDisplay g@(GameWorld rob@(Robot pos pm d visPoints blkPoints) (Grid gridPoints) obs) =
  GameDisplay 0 0 $ (fmap coordinateToCD) <$> gridPoints
  
  where
    visCDMap = fmap (pMToCCode False) visPoints
    blkCDMap = M.fromList $ zip obs (repeat Obs)
    notBlankCDMap = M.insert pos (pMToCCode True pm) $ visCDMap <> blkCDMap
    coordinateToCD coord@(Coordinate x y) = 
      case M.lookup coord notBlankCDMap of
        Just cc -> CellDetails x y cc
        Nothing -> CellDetails x y Blank


pMToCCode :: Bool -> PointMeta -> CellCode
pMToCCode True  (PointMeta True True True True)     = Robo 
pMToCCode True  (PointMeta True True True False)    = RoboL 
pMToCCode True  (PointMeta True True False True)    = RoboD 
pMToCCode True  (PointMeta True True False False)   = RoboDL 
pMToCCode True  (PointMeta True False True True)    = RoboR 
pMToCCode True  (PointMeta True False True False)   = RoboRL 
pMToCCode True  (PointMeta True False False True)   = RoboRD 
pMToCCode True  (PointMeta True False False False)  = RoboRDL 
pMToCCode True  (PointMeta False True True True)    = RoboU 
pMToCCode True  (PointMeta False True True False)   = RoboUL 
pMToCCode True  (PointMeta False True False True)   = RoboUD 
pMToCCode True  (PointMeta False True False False)  = RoboUDL 
pMToCCode True  (PointMeta False False True True)   = RoboUR 
pMToCCode True  (PointMeta False False True False)  = RoboURL 
pMToCCode True  (PointMeta False False False True)  = RoboURD 
pMToCCode True  (PointMeta False False False False) = RoboURDL
pMToCCode False (PointMeta True True True True)     = Vis 
pMToCCode False (PointMeta True True True False)    = VisL 
pMToCCode False (PointMeta True True False True)    = VisD 
pMToCCode False (PointMeta True True False False)   = VisDL 
pMToCCode False (PointMeta True False True True)    = VisR 
pMToCCode False (PointMeta True False True False)   = VisRL 
pMToCCode False (PointMeta True False False True)   = VisRD 
pMToCCode False (PointMeta True False False False)  = VisRDL 
pMToCCode False (PointMeta False True True True)    = VisU 
pMToCCode False (PointMeta False True True False)   = VisUL 
pMToCCode False (PointMeta False True False True)   = VisUD 
pMToCCode False (PointMeta False True False False)  = VisUDL 
pMToCCode False (PointMeta False False True True)   = VisUR 
pMToCCode False (PointMeta False False True False)  = VisURL 
pMToCCode False (PointMeta False False False True)  = VisURD 
pMToCCode False (PointMeta False False False False) = VisURDL
pMToCCode _     _                                   = Obs



-- gameWorldToStr :: GameWorld -> String
-- gameWorldToStr (GameWorld rob@(Robot pos pm d visPoints blkPoints) (Grid points) obs) = gridStr
--   where
--     initialGrid = fmap (fmap coordToStr) points
--     gridWithObs = foldl' (flip (changeWith obsStr)) initialGrid obs 
--     gridWithVisitedPoints = M.foldlWithKey' changePointMeta gridWithObs visPoints  
--     gridWithRobot = changeWith (robotToStr rob) pos gridWithVisitedPoints 
--     gridStr = unlines . reverse . map concat $ gridWithRobot
    
--     changeWith :: String -> Coordinate -> [[String]] -> [[String]]
--     changeWith newStr (Coordinate x y) ls = front ++ replaced : back
--       where
--         front = take (y-1) ls
--         back  = drop y ls
--         toBeReplacedList = ls !! (y-1)
--         replaced = (take (x-1) toBeReplacedList) ++ replacedStr : (drop x toBeReplacedList)
--         toBeReplacedStr = (ls !! (y-1)) !! (x-1)
--         replacedStr = (take 2 toBeReplacedStr) ++ newStr ++ (drop 4 toBeReplacedStr)
    
--     changePointMeta :: [[String]] -> Coordinate -> PointMeta -> [[String]]
--     changePointMeta ls (Coordinate x y) pm = front ++ replaced : back
--       where
--         front = take (y-1) ls
--         back  = drop y ls
--         toBeReplacedList = ls !! (y-1)
--         replaced = (take (x-1) toBeReplacedList) ++ replacedStr : (drop x toBeReplacedList)
--         toBeReplacedStr = (ls !! (y-1)) !! (x-1)
--         replacedStr = pointMetaToStr pm

-- obsStr :: String
-- obsStr = "@@"

-- robotToStr :: Robot -> String
-- robotToStr (Robot _ _ d _ _) = case d of
--     UP    -> "ß^"
--     RIGHT -> "ß»"
--     DOWN  -> "ßv"
--     LEFT  -> "«ß"

-- pointMetaToStr :: PointMeta -> String
-- pointMetaToStr (PointMeta True True True True)     = "  ©   "
-- pointMetaToStr (PointMeta False True True True)    = " `©   " 
-- pointMetaToStr (PointMeta True False True True)    = "  ©  |" 
-- pointMetaToStr (PointMeta True True False True)    = "  © _ " 
-- pointMetaToStr (PointMeta True True True False)    = "| ©   " 
-- pointMetaToStr (PointMeta False False True True)   = " `©  |" 
-- pointMetaToStr (PointMeta True False False True)   = "  © _|" 
-- pointMetaToStr (PointMeta True True False False)   = "| © _ " 
-- pointMetaToStr (PointMeta False True True False)   = "|`©   " 
-- pointMetaToStr (PointMeta True False True False)   = "| ©  |" 
-- pointMetaToStr (PointMeta False False False True)  = " `© _|" 
-- pointMetaToStr (PointMeta True False False False)  = "| © _|" 
-- pointMetaToStr (PointMeta False True False False)  = "|`© _ " 
-- pointMetaToStr (PointMeta False True False True )  = " `© _ " 
-- pointMetaToStr (PointMeta False False True False)  = "|`©  |" 
-- pointMetaToStr (PointMeta False False False False) = "|`© _|" 

-- coordToStr :: Coordinate -> String
-- coordToStr = const "  •   "