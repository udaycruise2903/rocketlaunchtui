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

-- Auto-convert database rows into Spacecrafts types for queries
--instance FromRow Spacecrafts where
--    fromRow = Datapoint <$> field <*> field

-- spacecrafts api
jsonURL :: String
jsonURL = "https://isro.vercel.app/api/spacecrafts"

getJSON :: IO BC.ByteString
getJSON = simpleHttp jsonURL

-- "insert" operation
-- Open a DB connection, create a table (maybe)
-- insert the value, close the connection
{-
insertSpacecraftIntoDB :: Spacecrafts -> IO ()
insertSpacecraftIntoDB (Spacecrafts {..}) =
 do conn <- open "test.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS spacecrafts (id INTEGER, name TEXT)"
    execute conn "INSERT INTO spacecrafts (id,name) VALUES (?,?)" (id,name)
    close conn
-}

-- A simple test to print out the whole table
{-
printDB :: IO ()
printDB =
 do conn <- open "test.db"
    res <- query_ conn "SELECT * FROM spacecrafts" :: IO [Spacecrafts]
    putStrLn (unlines (map show res))
    close conn
-}

-- GUI
ui :: Widget ()
ui = str "Hello World"

-- parse the json, insert to DB, print entire DB
main :: IO ()
main = do
  simpleMain ui
  d <- (eitherDecode <$> getJSON) :: IO (Either String Spacecrafts)
  case d of
    Left err     -> putStrLn err
    Right spacecrafts -> print spacecrafts
  --putStrLn "----- Database -----"
  --printDB