--
-- Rasterizer.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--


module Rasterizer
  ( module Rasterizer
  ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa (D, DIM2, Array, (:.) (..), Z(..), fromFunction, ix2, (!), ix1, Source, Structured (..))
import Data.List (zipWith4)

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Geometry

import Model

import Control.Lens ((^.),from,(%~),_1,(&),to)

import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)
import Graphics.Image.Interface.Repa (fromRepaArrayP)

type Raster r a = Array r DIM2 a

rasterizeBoundary :: (Structured t Double Double) => Rasterizer a t -> Rasterizer a (TR t)
rasterizeBoundary rasterizer = \sz box a -> R.smap (\t -> 4*t*(1-t)) $ rasterizer sz box a

--gaussianFilter :: Int -> Double -> Array D DIM2 

signSqrt :: Double -> Double
signSqrt x | x >= 0 = sqrt x
           | otherwise = -(sqrt (-x))

rasterizeCircle :: (Int, Int) -> Box -> Circle -> Raster D Double 
rasterizeCircle (nx,ny) box circ = 
  let 
    (center,radius) = circ^.circAsPair
    (cx,cy) = center^.ptAsPair
    (width,height) = box^.dimensions.vecAsPair
    (lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornIxLoc 
      = \(Z:.i:.j) -> 
          (boxLeft box + fromIntegral i*pixWidth,boxTop box - fromIntegral j*pixHeight)^.from ptAsPair
    cornxLoc = \(Z:.i) -> boxLeft box + fromIntegral i * pixWidth
    cornyLoc = \j -> boxTop box - fromIntegral j*pixHeight
    ylocInv = \h -> (boxTop box - h)/pixHeight
    upper = fromFunction (ix1 (nx+1)) $ \ix -> ylocInv $ cy+(signSqrt $ radius^2-(cornxLoc ix-cx)^2)
    lower = fromFunction (ix1 (nx+1)) $ \ix -> ylocInv $ cy-(signSqrt $ radius^2-(cornxLoc ix-cx)^2)
    corners =
      fromFunction (ix2 (nx+1) (ny+1)) 
        $ \(Z:.i:.j) ->
            let
              yl = fromIntegral j
              ix = ix1 i
            in
              if (yl < upper!ix) && (yl > lower!ix)
              then
                0.25
              else
                0
    vals =
      mapStencil2 
        BoundClamp
        [stencil2| 0 1 1
                   0 1 1
                   0 0 0 |]
        corners
  in
    R.extract (ix2 0 0) (ix2 nx ny) vals

--rasterizeConvexPolygon 

{-
rasterizeConvex :: (Int, Int) -> Box -> Region -> Raster D Double
rasterizeConvex (nx,ny) box region =
  let
    (width,height) = box^.dimensions.vecAsPair
    (lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornIxLoc 
      = \(Z:.i:.j) -> 
          (boxLeft box + fromIntegral i*pixWidth,boxTop box - fromIntegral j*pixHeight)^.from ptAsPair
    corners =
      fromFunction (ix2 (nx+1) (ny+1)) 
        $ (region^.inside) . cornIxLoc
    computeIntensity ((Z:.i):.j) = 
      let
        testIxs = [ix2 i j,ix2 (i+1) j,ix2 i (j+1),ix2 (i+1) (j+1)]
        inCorners = length $ filter (corners!) testIxs
      in
        if inCorners == 0
        then
          0
        else
          if inCorners == 4
          then
            1
          else
            fromIntegral inCorners/4
  in
    fromFunction (ix2 nx ny) computeIntensity
-}

-- rasterize rasterSize rasterRegion region -> antialiased region intensity
-- uses only corners even for antialiasing rn. Can be improved to higher quality msaa antialiasing
-- in the future.
rasterize :: (Int,Int) -> Box -> Region -> Raster D Double 
rasterize (nx,ny) box region =
  let
    (width,height) = box^.dimensions.vecAsPair
    (lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornIxLoc 
      = \(Z:.i:.j) -> 
          (boxLeft box + fromIntegral i*pixWidth,boxTop box - fromIntegral j*pixHeight)^.from ptAsPair
    corners =
      fromFunction (ix2 (nx+1) (ny+1)) 
        $ (region^.inside) . cornIxLoc
    computeIntensity ((Z:.i):.j) = 
      let
        testIxs = [ix2 i j,ix2 (i+1) j,ix2 i (j+1),ix2 (i+1) (j+1)]
        inCorners = length $ filter (corners!) testIxs
      in
        if inCorners == 0
        then
          0
        else
          if inCorners == 4
          then
            1
          else
            fromIntegral inCorners/4
          {- turning off more complicated antialiasing for now, currently having performance issues, so lets 
           - optimize the rest first, even though complicated antialiasing should now be fairly rare
            let
              corner = cornIxLoc (i,j)
              aaTests =
                map 
                  (& _1 %~ ((+.corner) . (^.from vecAsPair)))
                  (  (map (,1) [(0.5,-0.5),(0.25,-0.25),(0.25,-0.75),(0.75,-0.25),(0.75,-0.75)])
                  ++ (map (,0.5) [(0.5,0),(0,-0.5),(1,-0.5),(0.5,-1)])
                  )
              aaTot = 1 + (sum $ map snd aaTests)
              aaInsides = 
                (fromIntegral inCorners/4) 
                + 
                ( sum
                . map snd 
                . filter (^._1.to (region^.inside))
                $ aaTests
                )
            in
              aaInsides/aaTot
          -}
  in
    fromFunction (ix2 nx ny) computeIntensity

{-
-- screenx increases left to right
-- screeny increases top to bottom
-- 0,0 is the upper left hand corner of the upper left hand corner pixel
-- maxx,maxy is the lower right hand corner of the lower right hand corner pixel
-- i.e. there are maxx-1 and maxy-1 columns and rows of pixels respectively
screenToAbstract :: (Real a) => (a,a) -> Box -> (a,a) -> (Double,Double)
screenToAbstract (maxx,maxy) abstractBox (screenx,screeny) = 
  let
    zoX = (realToFrac screenx)/(realToFrac maxx) :: Double
    zoY = (realToFrac screeny)/(realToFrac maxy) :: Double
    (width,height) = abstractBox^.dimensions.vecAsPair
  in
    (boxLeft abstractBox + zoX*width, boxTop abstractBox - zoY*height)


pixelToAbstract :: (Real a) => (a,a) -> Box -> (a,a) -> (Int,Int) -> (Double,Double)
pixelToAbstract mx absBnds (pixx,pixy) (subpixx,subpixy) = 
  screenToAbstract mx absBnds (fromIntegral pixx + subpixx,fromIntegral pixy+subpixy)

abstractToScreen :: (Int,Int) -> Box -> (Double,Double) -> (Double,Double)
abstractToScreen (maxx,maxy) abstractBox (absx,absy) =
  let
    (width,height) = abstractBox^.dimensions.vecAsPair

  in
    

pixelAlignedBoundingBox :: (Int,Int) -> Box -> Box -> Box
pixelAlignedBoundingBox (nx,ny) bigBox boundBox = 
  let
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
  in
-}

type Rasterizer r t = (Int,Int) -> Box -> r -> Raster t Double

background :: (Int,Int) -> Raster D LRGBA
background (nx,ny) = fromFunction (ix2 nx ny) (const invisible)

type Compositor = LRGBA -> LRGBA -> LRGBA

render :: (Source r LRGBA, Source t Double) => Raster r LRGBA -> Compositor -> (Int,Int) -> Box -> a -> Fill -> Rasterizer a t -> Raster D LRGBA
render bg comp sz@(nx,ny) box reg fill rasterizer = 
  let
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    centIxLoc (Z:.i:.j) = 
      (boxLeft box + (fromIntegral i+0.5)*pixWidth,boxTop box - (fromIntegral j+0.5)*pixHeight)^.from ptAsPair
  in
    R.zipWith (flip comp) bg 
      $ R.traverse
          (rasterizer sz box reg)
          id
          (\lkup ix -> ((lkup ix)*.) . monoidMaybe . fill . centIxLoc $ ix)

data Rasterizable t where
  Rasterizable :: a -> Fill -> Rasterizer a t -> Compositor -> Rasterizable t

delayRasterizer :: (Source t Double) => Rasterizer a t -> Rasterizer a D
delayRasterizer rsteriz sz bx r = R.delay $ rsteriz sz bx r

delayRaster :: Source t Double => Rasterizable t -> Rasterizable D
delayRaster (Rasterizable x f r c) = Rasterizable x f (delayRasterizer r) c

renderLayered :: (Int,Int) -> Box -> [Rasterizable D] -> Raster D LRGBA
renderLayered sz _ [] = background sz
renderLayered sz box ((Rasterizable reg fill rast comp):lls) = 
  let 
    lflat = renderLayered sz box lls 
  in
    render lflat comp sz box reg fill rast 

toPixel :: LRGBA -> I.Pixel RGB Double
toPixel (LRGBA r g b _) = I.PixelRGB r g b

gammaCorrect :: I.Pixel RGB Double -> I.Pixel RGB Double
gammaCorrect = fmap (** (1/2.2))

convertToImage :: Raster D LRGBA -> Image RPU RGB Double
convertToImage raster =
  fromRepaArrayP $ R.map (gammaCorrect . toPixel) $ R.transpose raster 

defaultBox = makeBoxSides (-1) 1 (-1) 1
red = solidFill $ LRGBA 0.5 0 0 0.5
green = solidFill $ LRGBA 0 0.5 0 0.5
blue = solidFill $ LRGBA 0 0 0.5 0.5
orange = solidFill $ LRGBA 0.5 0.3 0 0.5
purple = solidFill $ LRGBA 0.5 0 0.5 0.5
grey = solidFill $ LRGBA 0.3 0.3 0.3 0.5
circ1 = ((-0.3,0.3)^.from ptAsPair,0.7)^.from circAsPair
circ2 = ((-0.3,-0.3)^.from ptAsPair,0.7)^.from circAsPair
circ3 = ((0.26,0)^.from ptAsPair,0.7)^.from circAsPair

circs = [circ1, circ2, circ3]
circRegs = (map circleRegion [circ1,circ2,circ3])

testLayers :: [Rasterizable D]
testLayers = zipWith4 (Rasterizable) 
  (circs ++ circs) 
  [orange,purple,grey,red,green,blue] 
  (map delayRasterizer ((replicate 3 (rasterizeBoundary rasterizeCircle)) ++ (replicate 3 rasterizeCircle)))
  (replicate 6 (mappend))

