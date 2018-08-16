--
-- CommonPaths.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.CommonPaths
  ( module Geometry.CommonPaths
  ) where

import Geometry.Types
import Geometry.Affine
import Geometry.Path

import Control.Lens ((^.))

xor = (/=)

makeRegularNGon :: Point -> Double -> Double -> Int -> Contour
makeRegularNGon center rad stphi n = 
  makeContour $ map f [0..(n-1)]
  where
    f i = 
      let
        dtheta = 2*pi/(fromIntegral n)
        thet = stphi + dtheta*(fromIntegral i)
        v = makeVector (rad * cos thet) (rad * sin thet)
      in
        PathSeg (center +. v)

-- center and xradius, yradius
-- starts at center + (xradius,yradius) can therefore control the starting point
-- should both have positive absolute value though
makeRectangle :: Point -> Double -> Double -> Bool -> Contour
makeRectangle cent xrad yrad direction = 
  let
    (cx,cy) = cent^.ptAsPair
    hfirst = (xrad > 0) `xor` (yrad > 0) `xor` direction
  in
    makeContour $ 
      if hfirst
      then
        [ PathSeg (makePoint (cx+xrad) (cy+yrad))
        , PathSeg (makePoint (cx-xrad) (cy+yrad))
        , PathSeg (makePoint (cx-xrad) (cy-yrad))
        , PathSeg (makePoint (cx+xrad) (cy-yrad))
        ]
      else
        [ PathSeg (makePoint (cx+xrad) (cy+yrad))
        , PathSeg (makePoint (cx+xrad) (cy-yrad))
        , PathSeg (makePoint (cx-xrad) (cy-yrad))
        , PathSeg (makePoint (cx-xrad) (cy+yrad))
        ]

type RCSpec = (Double,Double)

-- parameters :: 
-- point is the center of the rectangle
-- first double is x radius
-- next double is y radius
-- next is a quadruple of pairs of corner specifiers 
-- contour is produced in the positive direction starting with the right hand segment
--    _______ 
--
-- |           |
-- |           |
--      ______
--
makeRoundRectCorners :: Double -> Point -> Double -> Double -> (RCSpec,RCSpec,RCSpec,RCSpec) -> Contour
makeRoundRectCorners tol cent xrad yrad ((x1,y1),(x2,y2),(x3,y3),(x4,y4)) = 
  let
    xr = abs xrad
    yr = abs yrad
    (cx,cy) = cent^.ptAsPair
  in
    makeContour
      [ PathSeg $ makePoint (cx+xr) (cy-yr+y4)
      , makeArcSegment (makePoint (cx+xr) (cy+yr-y1)) (x1,y1,0) False True tol
      , PathSeg $ makePoint (cx+xr-x1) (cy+yr)
      , makeArcSegment (makePoint (cx-xr+x2) (cy+yr)) (x2,y2,0) False True tol
      , PathSeg $ makePoint (cx-xr) (cy+yr-y2)
      , makeArcSegment (makePoint (cx-xr) (cy-yr+y3)) (x3,y3,0) False True tol
      , PathSeg $ makePoint (cx-xr+x3) (cy-yr)
      , makeArcSegment (makePoint (cx+xr-x4) (cy-yr)) (x4,y4,0) False True tol
      ]

makeRoundRect :: Double -> Point -> Double -> Double -> RCSpec -> Contour
makeRoundRect tol cent xrad yrad rcs = makeRoundRectCorners tol cent xrad yrad (rcs,rcs,rcs,rcs)

makeRoundRectDiag :: Double -> Point -> Double -> Double -> (RCSpec,RCSpec) -> Contour
makeRoundRectDiag tol cent xrad yrad (rcs1,rcs2) = makeRoundRectCorners tol cent xrad yrad (rcs1,rcs2,rcs1,rcs2)

makeRoundRectCircles :: Double -> Point -> Double -> Double -> Double -> Contour
makeRoundRectCircles tol cent xrad yrad r = makeRoundRect tol cent xrad yrad (r,r)

makeRoundRectCircDiag :: Double -> Point -> Double -> Double -> Double -> Double -> Contour
makeRoundRectCircDiag tol cent xrad yrad r1 r2 = makeRoundRectDiag tol cent xrad yrad ((r1,r1),(r2,r2))
