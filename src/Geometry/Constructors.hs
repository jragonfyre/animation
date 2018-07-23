--
-- Constructors.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Constructors where

import Data.Array

import Geometry.Types
import Geometry.Common
import Geometry.Affine

{-

makePoint :: Double -> Double -> Point
makePoint = (,)

makeVector :: Double -> Double -> Vector
makeVector = (,)

makeCircle :: Point -> Double -> Circle
makeCircle = Circle

makePolyLine :: [Point] -> PolyLine
makePolyLine ps = PolyLine $ listArray (0,fromIntegral $ length ps - 1) ps


avgPoints :: [Point] -> Point
avgPoints ps = (1/fromIntegral (length ps)) *. foldr (+.) (0,0) ps

-- makePolygon vertices isConvex
makePolygon :: [Point] -> Bool -> Polygon
makePolygon ps isConvex = 
  let
    pl = makePolyLine (ps ++ [head ps])
    center = avgPoints ps
  in 
    Polygon 
      { polyBdry = pl
      , polyRegion =
          if isConvex
          then
            Just 
              . makeConvexPolytope
              . segmentFold
                  (uncurry (makeHalfPlanePoint center) . segmentToLine)
                  (:)
                  []
              $ pl
          else
            Nothing
      }


makeSegment :: Point -> Point -> Segment
makeSegment = (,)


-- the halfplane
-- v . x >= d
-- contructor normalizes v
makeHalfPlane :: Vector -> Double -> HalfPlane
makeHalfPlane v d =
  let
    s = vectorNorm v
  in 
    HalfPlane (s *. v) (s*d)

-- guarantees the point is in the halfplane
makeHalfPlanePoint :: Point -> Vector -> Double -> HalfPlane
makeHalfPlanePoint p v d = 
  if p `dot` v >= d
  then
    makeHalfPlane v d
  else 
    makeHalfPlane ((0,0)-.v) (-d)

makeConvexPolytope :: [HalfPlane] -> ConvexPolytope
makeConvexPolytope = ConvexPolytope


-}
