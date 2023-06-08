{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
import Miso.String
import Elmlens
import Apps
import Todo

main :: IO ()
main = startApp $ 
  -- todo_app_without_filter 
  -- highlight_demo_app
   -- todomvcapp $ Nothing
  -- themedApp $ Nothing
  themedApp' $ Nothing

