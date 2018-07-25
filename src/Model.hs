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

monoidMaybe :: (Monoid m) => Maybe m -> m
monoidMaybe Nothing = mempty
monoidMaybe (Just m) = m

-- alpha is premultiplied so we can do linear blending (specifically for antialiasing).
data LRGBA = LRGBA Double Double Double Double
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

icPix :: Pixel -> Integer -> Region -> Double
icPix pix s r = implicitCurvePix pix s s r

implicitCurvePix :: Pixel -> Integer -> Integer -> Region -> Double
implicitCurvePix pix nv nh r = 
  let 
    t = antialiasPixelIntensity pix nv nh r
  in
    (-4)*t*(t-1)


aaPix :: Pixel -> Integer -> Region -> Double
aaPix pix s r = antialiasPixelIntensity pix s s r

antialiasPixelIntensity :: Pixel -> Integer -> Integer -> Region -> Double
antialiasPixelIntensity pix numSamplesV numSamplesH region =
  let
    (sx,sy) = pix^.corner.ptAsPair
    (pixWidth,pixHeight) = pix^.dimensions.vecAsPair
    nv = numSamplesV-1
    nh = numSamplesH-1
    dx = pixWidth/fromIntegral nh
    dy = pixHeight/fromIntegral nv
    testpts = [ ptFromPair (sx+(fromIntegral i)*dx,sy+(fromIntegral j)*dy) | i <- [0..nh], j <- [0..nv]]
  in
    (/(fromIntegral $ numSamplesV*numSamplesH))
      . fromIntegral 
      . length 
      . filter (region^.inside)
      $ testpts

multisampleAntialiaser :: Integer -> Integer -> Antialiaser
multisampleAntialiaser numSamplesV numSamplesH fill pix =
  let
    (sx,sy) = pix^.corner.ptAsPair
    (pixWidth,pixHeight) = pix^.dimensions.vecAsPair
    nv = numSamplesV-1
    nh = numSamplesH-1
    dx = pixWidth/fromIntegral nh
    dy = pixHeight/fromIntegral nv
    testpts = [ ptFromPair (sx+(fromIntegral i)*dx,sy+(fromIntegral j)*dy) | i <- [0..nh], j <- [0..nv]]
  in
    ((1/(fromIntegral $ numSamplesV*numSamplesH) :: Double) *.)
      . foldr (+.) invisible  
      . mapMaybe fill
      $ testpts

msaa :: Integer -> Antialiaser
msaa n = multisampleAntialiaser n n

-- takes postmultipliedAlpha
makeLRGBA :: Double -> Double -> Double -> Double -> LRGBA
makeLRGBA r g b a = LRGBA (r*a) (g*a) (b*a) a

type Fill = Point -> Maybe LRGBA

solidFill :: LRGBA -> Fill
solidFill c _ = Just c

filledRegion :: Fill -> Region -> Fill
filledRegion f r pt = if (r^.inside) pt then f pt else Nothing

type Pixel = Box

type Renderer = Pixel -> LRGBA

type Antialiaser = Fill -> Renderer

--type PixelRenderer :: 


{-

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

-}
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
      





