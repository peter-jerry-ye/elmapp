module Main where

import Miso
import System.Random
import Benchbase

main :: IO ()
main = do 
  seed <- initStdGen
  startApp $ benchbaseApp seed

