--
-- Rasterizer.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Rasterizer
  ( module Rasterizer
  ) where

import Data.Array
import Data.List (zip4)

import Geometry

import Model

import Control.Lens ((^.),from,(%~),_1,(&),to)

import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)

type Raster a = Array (Int,Int) a

rasterizeBoundary :: (Int,Int) -> Box -> Region -> Raster Double
rasterizeBoundary sz box rg = fmap (\t -> 4*t*(1-t)) $ rasterize sz box rg

-- rasterize rasterSize rasterRegion region -> antialiased region intensity
-- uses only corners even for antialiasing rn. Can be improved to higher quality msaa antialiasing
-- in the future.
rasterize :: (Int,Int) -> Box -> Region -> Raster Double 
rasterize (nx,ny) box region =
  let
    (width,height) = box^.dimensions.vecAsPair
    (lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornIxLoc = \(i,j) -> (boxLeft box + fromIntegral i*pixWidth,boxTop box - fromIntegral j*pixHeight)^.from ptAsPair
    corners =
      array
        ((0,0),(nx,ny))
        [ ((i,j),(region^.inside) (cornIxLoc (i,j))) 
        | i <- [0..nx]
        , j <- [0..ny]
        ]
    computeIntensity (i,j) = 
      let
        testIxs = [(i,j),(i+1,j),(i,j+1),(i+1,j+1)]
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
    array
      ((0,0),(nx-1,ny-1))
      [ ((i,j),computeIntensity (i,j)) 
      | i <- [0..nx-1]
      , j <- [0..ny-1]
      ]

{-
screenToAbstract :: (Real a) => (a,a) -> Box -> (a,a) -> (Double,Double)
screenToAbstract (row,column)

abstractToScreen :: (Double,Double) -> (Double,Double)

pixelAlignedBoundingBox :: (Int,Int) -> Box -> Box -> Box
pixelAlignedBoundingBox (nx,ny) bigBox boundBox = 
  let
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
  in
-}

type Rasterizer = (Int,Int) -> Box -> Region -> Raster Double

background :: (Int,Int) -> Raster LRGBA
background (nx,ny) = array ((0,0),(nx-1,ny-1)) [((i,j),invisible) | i <- [0..nx-1], j <- [0..ny-1]]

type Compositor = LRGBA -> LRGBA -> LRGBA

render :: Raster LRGBA -> Compositor -> (Int,Int) -> Box -> Region -> Fill -> Rasterizer -> Raster LRGBA
render bg comp sz@(nx,ny) box reg fill rasterizer = 
  let
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    centIxLoc (i,j) = 
      (boxLeft box + (fromIntegral i+0.5)*pixWidth,boxTop box - (fromIntegral j+0.5)*pixHeight)^.from ptAsPair
  in
    accum (flip comp) bg . fmap (\(ix,s) -> (ix,) . (s*.) . monoidMaybe . fill . centIxLoc $ ix) . assocs $ rasterizer sz box reg

renderLayered :: (Int,Int) -> Box -> [(Region,Fill,Rasterizer,Compositor)] -> Raster LRGBA
renderLayered sz _ [] = background sz
renderLayered sz box ((reg,fill,rast,comp):lls) = 
  let 
    lflat = renderLayered sz box lls 
  in
    render lflat comp sz box reg fill rast 

toPixel :: LRGBA -> I.Pixel RGB Double
toPixel (LRGBA r g b _) = I.PixelRGB r g b

gammaCorrect :: I.Pixel RGB Double -> I.Pixel RGB Double
gammaCorrect = fmap (** (1/2.2))

convertToImage :: Raster LRGBA -> Image RPU RGB Double
convertToImage raster =
  let 
    (_,(mx,my)) = bounds raster 
    dimens = (my+1,mx+1)
  in
    I.makeImageR I.RPU dimens (\(i,j) -> gammaCorrect . toPixel $ raster!(j,i))

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

circRegs = (map circleRegion [circ1,circ2,circ3])

testLayers :: [(Region,Fill,Rasterizer,Compositor)]
testLayers = zip4 (circRegs ++ circRegs) [orange,purple,grey,red,green,blue] 
  ((replicate 3 rasterizeBoundary) ++ (replicate 3 rasterize)) (replicate 6 (+.))
