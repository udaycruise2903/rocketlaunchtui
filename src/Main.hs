{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import GHC.Generics                     
import Data.Aeson                      
import Database.SQLite.Simple           
import Data.Text (Text,unpack)
import qualified Data.ByteString.Lazy.Char8 as BC 

-- The type received from JSON
data Spacecraft = Spacecraft
           { id             :: Integer
           , name           :: Text
           }
           deriving (Eq, Show, Read, Generic, FromJSON, ToJSON )

-- Auto-convert database rows into Spacecraft types for queries
instance FromRow Spacecraft where
    fromRow = Spacecraft <$> field <*> field

-- "insert" operation
-- Open a DB connection, create a table (maybe), insert the value, close the
-- connection
insertSpacecraftIntoDB :: Spacecraft -> IO ()
insertSpacecraftIntoDB (Spacecraft {..}) =
 do conn <- open "test.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS spacecraft (id INTEGER, name TEXT)"
    execute conn "INSERT INTO spacecraft (id,name) VALUES (?,?)" (id,name)
    close conn

-- A simple test to print out the whole table
printDB :: IO ()
printDB =
 do conn <- open "test.db"
    res <- query_ conn "SELECT * FROM spacecraft" :: IO [Spacecraft]
    putStrLn (unlines (map show res))
    close conn

-- Glue it all together by
-- 1. Make the json 2. parse the json 3. insert to DB 4. print entire DB
main :: IO ()
main =
  do let spacecraft_json = encode (Spacecraft 1 "Aryabhata")
     putStrLn $ "JSON: " ++ BC.unpack spacecraft_json
     case eitherDecode spacecraft_json of
        Left err     -> error err
        Right spacecraft -> insertSpacecraftIntoDB spacecraft
     putStrLn "----- Database -----"
     printDB