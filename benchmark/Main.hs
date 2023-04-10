module Main where

import Miso
import System.Random
import Benchmark

main :: IO ()
main = do 
  seed <- initStdGen
  startApp $ benchmarkApp seed Nothing

