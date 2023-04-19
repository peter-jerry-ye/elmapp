{-# LANGUAGE TypeFamilies #-}
module Main where

import Test.QuickCheck
import ElmlensProp
import Elmlens
import Apps

instance MaskedUpdateStructure IntU where
  type Mask IntU = ()
  mask _ _ = ()
  eqv _ _ = (==)
  

proj2L' :: ULens (ProdU IntU IntU) IntU
proj2L' = proj2L 0

main :: IO ()
main = do
  quickCheck $ consistencyProp proj2L'
  quickCheck $ createProp proj2L'
  quickCheck $ translateEmptyProp proj2L'
  quickCheck $ translateCombineProp proj2L'
