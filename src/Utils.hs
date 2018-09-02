--
-- Utils.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Utils
  ( module Utils
  ) where

import Control.Lens
import Control.Lens.TH (defaultFieldRules, generateUpdateableOptics)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as I
import Data.Function (on)

onlyGetters = defaultFieldRules & generateUpdateableOptics .~ False

minMaxOf :: (Ord a) => [a] -> (a,a)
minMaxOf (x:xs) = mmh x x xs
  where
    mmh min max [] = (min,max)
    mmh min max (y:ys) = 
      if y < min
      then
        mmh y max ys
      else
        if y > max
        then
          mmh min y ys
        else
          mmh min max ys

vSortBy :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
vSortBy comp vec = V.create $ do
  mv <- V.thaw vec
  I.sortBy comp mv
  return mv

vSortOn :: (Ord b) => (a->b) -> V.Vector a -> V.Vector a
vSortOn f = vSortBy (compare `on` f)

vSort :: (Ord a) => V.Vector a -> V.Vector a
vSort = vSortBy compare

indicate :: Bool -> Double
indicate True = 1
indicate False = 0

monoidMaybe :: (Monoid m) => Maybe m -> m
monoidMaybe Nothing = mempty
monoidMaybe (Just m) = m

-- doesn't do boundary checks
invertMonotoneIncreasing :: Double -> (Double, Double) -> (Double -> Double) -> Double -> Double
invertMonotoneIncreasing tol (st,ed) f y = 
  let 
    mid = (ed+st)/2
    val = f mid
  in
    if (ed - st) < tol
    then
      mid
    else
      invertMonotoneIncreasing
        tol
        ( if f mid < y
          then
            (mid,ed)
          else
            (st,mid)
        )
        f
        y


