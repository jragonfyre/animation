{-# OPTIONS_GHC -fobject-code #-}
--
-- FinVector.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module FinVector
  ( module FinVector
  ) where

import GHC.Exts
import MathClasses
import Geometry.Types
import Geometry.Affine

data VSDim 
  = VSZero
  | VSOne 
  | VSTwo
  -- | VSThree
  -- | VSFour

class (Vectorlike (VSpace a)) => VSpaceClass (a :: VSDim) where
  type VSpace a = (c :: *) | c -> a
  dimension :: VSpace a -> VSDim

instance VSpaceClass VSZero where
  type VSpace VSZero = ()
  dimension _ = VSZero
instance VSpaceClass VSOne where
  type VSpace VSOne = Double
  dimension _ = VSOne
instance VSpaceClass VSTwo where
  type VSpace VSTwo = Vector
  dimension _ = VSTwo



{-
type V2 = DoubleX2#

mkV2 :: Double# -> Double# -> V2
mkV2 x y = packDoubleX2# (# x, y #)

(+.#) :: V2 -> V2 -> V2
(+.#) = plusDoubleX2#

-}
{-
type family VSpace (a :: VSDim) where
  VSpace VSZero = ()
  VSpace VSOne = Double#
  VSpace VSTwo = (# Double#, Double# #)
  VSpace VSThree = (Double,Double,Double)
  --VSpace VSFour = 
-}
