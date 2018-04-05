--
-- Types.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Types where
  --( 
  --) where

import Data.Array (Array)


-- point
type Point = (Double, Double)

type Vector = (Double, Double)

type M2 = (Vector, Vector)

type A2 = (M2,Vector)

-- line segment
type Segment = (Point,Point)

-- open polygon
newtype PolyLine = PolyLine (Array Integer Point)

-- closed polygon
-- is a polyline whose first and last points are the same
data Polygon = Polygon 
  { polyBdry :: PolyLine 
  , polyRegion :: Maybe ConvexPolytope
  }

data Circle = Circle 
  { center :: Point
  , radius :: Double
  }

data HalfPlane = HalfPlane Vector Double

data ConvexPolytope = ConvexPolytope [HalfPlane]

data ImplicitRegion = ImplicitRegion (Point -> Bool) 

