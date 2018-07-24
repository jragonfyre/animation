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

-- alpha is premultiplied so we can do linear blending (specifically for antialiasing).
data LRGBA = LRGBA Double Double Double Double
  deriving (Show, Read, Eq, Ord)

icPix :: (Region r) => Point -> Double -> Integer -> RegionData r -> r -> Double
icPix c w s val r = implicitCurvePix c w w s s val r

implicitCurvePix :: (Region r) => 
  Point -> Double -> Double -> Integer -> Integer -> RegionData r -> r -> Double
implicitCurvePix c w h nv nh v r = 
  let 
    t = antialiasPixelIntensity c w h nv nh v r
  in
    (-4)*t*(t-1)

aaPix :: (Region r) => Point -> Double -> Integer -> RegionData r -> r -> Double
aaPix c w s val r = antialiasPixelIntensity c w w s s val r

antialiasPixelIntensity :: (Region r) =>
  Point -> Double -> Double -> Integer -> Integer -> RegionData r -> r -> Double
antialiasPixelIntensity center pixelWidth pixelHeight numSamplesV numSamplesH val region =
  let
    (sx,sy) = (center -. (1/2 :: Double)*.(ptFromPair (pixelWidth, pixelHeight)))^.vecAsPair
    nv = numSamplesV-1
    nh = numSamplesH-1
    dx = pixelWidth/fromIntegral nh
    dy = pixelWidth/fromIntegral nv
    testpts = [ ptFromPair (sx+(fromIntegral i)*dx,sy+(fromIntegral j)*dy) | i <- [0..nh], j <- [0..nv]]
  in
    (/(fromIntegral $ numSamplesV*numSamplesH)) 
      . fromIntegral 
      . length 
      . filter (inside val region)
      $ testpts


-- takes postmultipliedAlpha
makeLRGBA :: Double -> Double -> Double -> Double -> LRGBA
makeLRGBA r g b a = LRGBA (r*a) (g*a) (b*a) a

class Fill f where
  type FillData f :: *
  colorPt :: FillData f -> f -> Point -> Maybe LRGBA

newtype SolidFill = SolidFill LRGBA
  deriving (Show, Read, Eq, Ord)

instance Fill SolidFill where
  type FillData SolidFill = ()
  colorPt () (SolidFill c) _ = Just c

data FilledRegion r f where
  FilledRegion :: (Region r, Fill f) => r -> f -> FilledRegion r f

instance Drawable (FilledRegion r f) where
  type DrawData (FilledRegion r f) = (RegionData r, FillData f)
  getPixel (rdat,fdat) (FilledRegion r f) pixw pixh pxlc =
    let
      (pixx,pixy) = pxlc^.ptAsPair
      pixc = (pixx+pixw/2,pixy+pixh/2)^.from ptAsPair
    in
      if inside rdat r pixc
      then
        colorPt fdat f pixc
      else
        Nothing

data SolidCurve r c f where
  SolidCurve :: (Region r, ImplicitCurve c, Fill f) => r -> c -> f -> SolidCurve r c f

{-
instance Drawable (SolidCurve r c f) where
  type DrawData (SolidCurve r c f) = (RegionData r, FillData f, Double, Integer)
  getPixel (rdat, fdat, pscale, numSamples) (SolidCurve r c f) pw ph pllc = 
    let
      (pixx,pixy)=pllc^.ptAsPair
      pixc = (pixx+pw/2,pixy+ph/2)^.from ptAsPair
    in
      if inside rdat r pixc
      then
        do
          (LRGBA r g b a) <- colorPt fdat f pixc
          let s = implicitCurvePix 
                  pixc
                  (pscale*pw)
                  (pscale*ph)
                  numSamples
                  numSamples
                  ()
                  (ImplicitRegion ((>=0) . implicit c))
          return $ LRGBA (r*s) (g*s) (b*s) (a*s)
      else
        Nothing
-}
      


data DrawBox :: * where
  DrawBox :: (Drawable d) => DrawData d -> d -> DrawBox

instance Drawable DrawBox where
  type DrawData DrawBox = ()
  getPixel () (DrawBox dat d) = getPixel dat d

instance Drawable [DrawBox] where
  type DrawData [DrawBox] = ()
  getPixel () [] _ _ _ = Nothing
  getPixel () (d:ds) pw ph pllc = case getPixel () d pw ph pllc of 
    Just x ->
      Just x
    Nothing ->
      getPixel () ds pw ph pllc

class Drawable d where
  type DrawData d :: *
  -- getPixel pixWidth pixHeight pixLowerLeftCorner Maybe color
  getPixel :: DrawData d -> d -> Double -> Double -> Point -> Maybe LRGBA

getPixelNull :: (Drawable d, DrawData d ~ ()) => d -> Double -> Double -> Point -> Maybe LRGBA
getPixelNull = getPixel ()


