module Main where

import Brick (Widget, simpleMain, str)
-- import qualified Data.ByteString.Lazy as B


ui :: Widget ()
ui = str "Hello World"

main :: IO ()
main = simpleMain ui
