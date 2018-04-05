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

-- alpha is premultiplied so we can do linear blending (specifically for antialiasing).
data LRGBA = LRGBA Double Double Double Double

-- takes postmultipliedAlpha
makeLRGBA :: Double -> Double -> Double -> Double -> LRGBA
makeLRGBA r g b a = LRGBA (r*a) (g*a) (b*a) a

class Fill f where
  type FillData f :: *
  colorPt :: FillData f -> f -> Point -> Maybe LRGBA

data FilledRegion r f where
  FilledRegion :: (Region r, Fill f) => r -> f -> FilledRegion r f

instance Drawable (FilledRegion r f) where
  type DrawData (FilledRegion r f) = (RegionData r, FillData f)
  getPixel (rdat,fdat) (FilledRegion r f) pixw pixh (pixx,pixy) =
    let
      pixc = (pixx+pixw/2,pixy+pixh/2)
    in
      if inside rdat r pixc
      then
        colorPt fdat f pixc
      else
        Nothing


data DrawBox :: * where
  DrawBox :: (Drawable d) => DrawData d -> d -> DrawBox

instance Drawable DrawBox where
  type DrawData DrawBox = ()
  getPixel () (DrawBox dat d) = getPixel dat d

class Drawable d where
  type DrawData d :: *
  -- getPixel pixWidth pixHeight pixLowerLeftCorner Maybe color
  getPixel :: DrawData d -> d -> Double -> Double -> Point -> Maybe LRGBA

getPixelNull :: (Drawable d, DrawData d ~ ()) => d -> Double -> Double -> Point -> Maybe LRGBA
getPixelNull = getPixel ()


