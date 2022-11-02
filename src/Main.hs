{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Prelude hiding (id)
import GHC.Generics                     
import Data.Aeson                      
import Database.SQLite.Simple           
import Data.Text (Text,unpack)
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as BC 

import Brick (Widget, simpleMain, str)

-- The type received from JSON
data DataPoint = DataPoint 
          { id :: Integer
          , name :: Text
          } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Spacecrafts = Spacecrafts 
          { spacecrafts :: [DataPoint]
          } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON )

-- spacecrafts api
jsonURL :: String
jsonURL = "https://isro.vercel.app/api/spacecrafts"

getJSON :: IO BC.ByteString
getJSON = simpleHttp jsonURL

-- GUI
ui :: Widget ()
ui = str "Welcome"

-- parse the json, insert to DB, print entire DB
main :: IO ()
main = do
  simpleMain ui
  d <- (eitherDecode <$> getJSON) :: IO (Either String Spacecrafts)
  case d of
    Left err     -> putStrLn err
    Right spacecrafts -> print spacecrafts
