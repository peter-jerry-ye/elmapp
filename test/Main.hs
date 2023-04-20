{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.HUnit
import Control.Category (Category (..))
import Prelude hiding (id)
import Miso
import qualified Miso.Html        as H
import ElmlensProp
import Elmlens
import Apps
import Benchmark
import System.Random

instance MaskedUpdateStructure IntU where
  type Mask IntU = ()
  mask _ _ = ()
  eqv _ _ = (==)

instance Eq a => MaskedUpdateStructure (RepU a) where
  type Mask (RepU a) = ()
  mask _ _ = ()
  eqv _ _ = (==)
  
instance (Monoid a, Eq a) => MaskedUpdateStructure (UnitU a) where
  type Mask (UnitU a) = ()
  mask _ _ = ()
  eqv _ _ = (==)

instance MaskedUpdateStructure LabelU where
  type Mask LabelU = ()
  mask _ _ = ()
  eqv _ _ = (==)

instance Arbitrary StdGen where
  arbitrary = fmap mkStdGen chooseAny

proj2L' :: ULens (ProdU IntU IntU) IntU
proj2L' = proj2L 0

testULens lens = do
  quickCheck $ consistencyProp lens
  quickCheck $ createProp lens
  quickCheck $ translateEmptyProp lens
  quickCheck $ translateCombineProp lens

testElmApp (ElmApp lens _) = testULens lens

test1 = TestCase (assertEqual "1==1" 1 1)

elmlens1 :: ElmApp IntU IntU HTML
elmlens1 = fromView $ \_ -> Base $ H.div_ [] []

elmlens2 = vmix elmlens1 elmlens1

main :: IO ()
main = do
  testElmApp elmlens2
  -- testElmApp buttons
  -- runTestTTAndExit $ TestList [TestLabel "test1" test1]