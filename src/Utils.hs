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
