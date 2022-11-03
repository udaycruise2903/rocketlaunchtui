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
          { id :: Text
          , country :: Text
          , launch_date :: Text
          , mass :: Text
          , launcher :: Text
          } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data Satellites = Satellites 
          { customer_satellites :: [DataPoint]
          } deriving (Eq, Show, Read, Generic, FromJSON, ToJSON )

-- satellites api
jsonURL :: String
jsonURL = "https://isro.vercel.app/api/customer_satellites"

getJSON :: IO BC.ByteString
getJSON = simpleHttp jsonURL

-- GUI
ui :: Widget ()
ui = str "Customer satellites launched by ISRO"

-- parse the json, insert to DB, print entire DB
main :: IO ()
main = do
  simpleMain ui
  s <- (eitherDecode <$> getJSON) :: IO (Either String Satellites)
  case s of
      Left err     -> putStrLn err
      Right satellites -> print satellites