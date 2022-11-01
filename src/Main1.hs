module Main where

import Brick (Widget, simpleMain, str)


ui :: Widget ()
ui = str "Hello World"

main :: IO ()
main = simpleMain ui
