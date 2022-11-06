{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Prelude hiding (id)
import GHC.Generics                     
import Data.Aeson                      
--import Database.SQLite.Simple           
import Data.Text (Text,unpack)
import Control.Applicative
import Control.Monad
import Network.HTTP.Simple
--import qualified Data.ByteString.Lazy.Char8 as BC 
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core


main :: IO ()
main = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

getSatelliteList :: IO SatelliteList
getSatelliteList = do
    request <-parseRequest "https://isro.vercel.app/api/customer_satellite" 
    resp <- httpJSON request
    return (getResponseBody resp :: SatelliteList)

data TuiState =
    TuiState SatelliteList
    deriving (Show)
  
type ResourceName = String
  
tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty []
        }
  
buildInitialState :: IO TuiState
buildInitialState = TuiState <$> getSatelliteList
  
drawTui :: TuiState -> [Widget ResourceName]
drawTui (TuiState (SatelliteList satellites)) = (:[]) . viewport "cards" Vertical . vBox . map (str . id) $ satellites
  
handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = halt s
  
instance FromJSON Satellite where
    parseJSON = withObject "Satellite" $ \v -> do 
        id <- v .: "id"
        country <- v .: "country"
        launch_date <- v .: "launch_date"
        mass <- v .: "mass"
        launcher <- v .: "launcher" 
        return $ Satellite id country launch_date mass launcher
  
  
instance FromJSON SatelliteList where
    parseJSON = withObject "SatelliteList" $ \v -> do
            satelliteListWrapper <- v .: "satellite_list"
            satellites <- satelliteListWrapper .: "satellites"
            return $ SatelliteList satellites
  
-- here to help with decoding from JSON
newtype SatelliteList = SatelliteList [Satellite] deriving (Show)
  
data Satellite = Satellite 
                  { id :: String
                  , country :: String
                  , launch_date :: String
                  , mass :: String
                  , launcher :: String
                  } deriving (Show)