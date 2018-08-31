module Spec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.SmallCheck as SC
import Test.QuickCheck as QC

import Geometry

--theTest :: Test
--theTest = 
  --TestLabel
    --"Whole Test"
    --$ TestList 
        --[
        --]

tests :: TestTree
tests = testGroup "Tests" [] --[properties, unitTests]

--properties :: TestTree

main :: IO ()
main = defaultMain tests
