{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types where
    
import Data.Aeson
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

import GE.Types

data GameDisplay = GameDisplay {
    gdNumCol  :: Int
  , gdNumRows :: Int
  , gdRows    :: [[CellDetails]]
  } deriving (Show, Eq, Generic) 

instance FromJSON GameDisplay
instance ToJSON GameDisplay

data CellDetails = CellDetails {
    cX       :: Int
  , cY       :: Int
  , cellCode :: CellCode
  }  deriving (Show, Eq, Generic) 


instance FromJSON CellDetails
instance ToJSON CellDetails

data CellCode =
    Blank 
  | Robo 
  | RoboL 
  | RoboD 
  | RoboDL 
  | RoboR 
  | RoboRL 
  | RoboRD 
  | RoboRDL 
  | RoboU 
  | RoboUL 
  | RoboUD 
  | RoboUDL 
  | RoboUR 
  | RoboURL 
  | RoboURD 
  | RoboURDL 
  | Vis 
  | VisL 
  | VisD 
  | VisDL 
  | VisR 
  | VisRL 
  | VisRD 
  | VisRDL 
  | VisU 
  | VisUL 
  | VisUD 
  | VisUDL 
  | VisUR 
  | VisURL 
  | VisURD 
  | VisURDL
  | Obs
  deriving (Show, Eq, Generic, Enum)

instance FromJSON CellCode
instance ToJSON CellCode

instance FromJSON Direction
instance ToJSON Direction