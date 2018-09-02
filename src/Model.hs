--
-- Model.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Model where
  --(
  --) where

import Geometry
import Control.Lens ((^.))
import Control.Lens.Iso (from)

import Data.Maybe (mapMaybe)

import Utils

-- alpha is premultiplied so we can do linear blending (specifically for antialiasing).
data LRGBA = LRGBA {-# UNPACK #-} !Double !Double !Double !Double
  deriving (Show, Read, Eq, Ord)

invisible :: LRGBA
invisible = LRGBA 0 0 0 0

-- associative alpha blending (assuming premultiplied alpha)
compose :: LRGBA -> LRGBA -> LRGBA
compose (LRGBA r g b a) (LRGBA x y z w) = LRGBA (r+(1-a)*x) (g+(1-a)*y) (b+(1-a)*z) (a+w-a*w)

instance Monoid LRGBA where
  mempty = invisible
  mappend = compose

instance Summable LRGBA LRGBA LRGBA where
  (+.) (LRGBA r g b a) (LRGBA x y z w) = LRGBA (r+x) (g+y) (b+z) (a+w)

instance Multiplicable Double LRGBA LRGBA where
  (*.) x (LRGBA r g b a) = LRGBA (x*r) (x*g) (x*b) (x*a)

-- takes postmultipliedAlpha
makeLRGBA :: Double -> Double -> Double -> Double -> LRGBA
makeLRGBA r g b a = LRGBA (r*a) (g*a) (b*a) a

type Fill = Point -> LRGBA

solidFill :: LRGBA -> Fill
solidFill = const



