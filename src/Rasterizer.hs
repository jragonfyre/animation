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

import Geometry

import Model

import Control.Lens ((^.),from,(%~),_1,(&),to)

import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)

type Raster a = Array (Int,Int) a

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
  in
    array
      ((0,0),(nx-1,ny-1))
      [ ((i,j),computeIntensity (i,j)) 
      | i <- [0..nx-1]
      , j <- [0..ny-1]
      ]


render :: (Int,Int) -> Box -> Region -> Fill -> Raster LRGBA
render sz@(nx,ny) box reg fill = 
  let
    (width,height) = box^.dimensions.vecAsPair
    (lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    centIxLoc (i,j) = (boxLeft box + (fromIntegral i+0.5)*pixWidth,boxTop box - (fromIntegral j+0.5)*pixHeight)^.from ptAsPair
  in
    array ((0,0),(nx-1,ny-1)) . fmap (\(ix,s) -> (ix,) . (s*.) . monoidMaybe . fill . centIxLoc $ ix) . assocs $ rasterize sz box reg

renderLayered :: (Int,Int) -> Box -> [(Region,Fill)] -> Raster LRGBA
renderLayered sz@(nx,ny) box layers = 
  let 
    renderedLayers = map (uncurry (render sz box)) layers
    compositedPix ix = mconcat $ map (!ix) renderedLayers
  in
    array ((0,0),(nx-1,ny-1)) [ ((i,j), compositedPix (i,j)) | i <- [0..nx-1], j <- [0..ny-1]]

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
circ1 = ((-0.3,0.3)^.from ptAsPair,0.7)^.from circAsPair
circ2 = ((-0.3,-0.3)^.from ptAsPair,0.7)^.from circAsPair
circ3 = ((0.26,0)^.from ptAsPair,0.7)^.from circAsPair

testLayers :: [(Region,Fill)]
testLayers = zip (map circleRegion [circ1,circ2,circ3]) [red,green,blue]
