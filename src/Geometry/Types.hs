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
import GHC.Generics

-- Lens imports
import Control.Lens
import Control.Lens.TH
import Control.Lens.Each
import Control.Lens.Fold (folding)
import Control.Lens.Iso (iso)

import Data.Array (elems, listArray)


-- point
--type Point = (Double, Double)

--Lens version of Point
data Point = Point 
  { _pointX, _pointY :: Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makePoint :: Double -> Double -> Point
makePoint = Point

ptToPair :: Point -> (Double,Double)
ptToPair Point{_pointX=x,_pointY=y} = (x,y)

ptFromPair :: (Double,Double) -> Point
ptFromPair = uncurry makePoint

makeFields ''Point

-- ptAsPair :: Iso' Point (Double, Double)
ptAsPair :: (Functor f, Profunctor p) => 
  p (Double, Double) (f (Double, Double)) -> p Point (f Point)
ptAsPair = iso ptToPair ptFromPair

instance Each Point Point Double Double where
  -- each :: Traversal' Point Double
  -- Applicative f => (Double -> f Double) -> Point -> f Point
  each inj Point{_pointX=x,_pointY=y} = Point <$> inj x <*> inj y

--Lens version
data Vector = Vector 
  { _vectorX, _vectorY :: Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeVector :: Double -> Double -> Vector
makeVector = Vector

vecToPair :: Vector -> (Double,Double)
vecToPair Vector{_vectorX=x,_vectorY=y} = (x,y)

vecFromPair :: (Double,Double) -> Vector
vecFromPair = uncurry makeVector

makeFields ''Vector

instance Each Vector Vector Double Double where
  -- each :: Traversal' Vector Double
  each inj Vector{_vectorX=x,_vectorY=y} = Vector <$> inj x <*> inj y

-- vecAsPair :: Iso' Vector (Double, Double)
vecAsPair :: (Functor f, Profunctor p) => 
  p (Double, Double) (f (Double, Double)) -> p Vector (f Vector)
vecAsPair = iso vecToPair vecFromPair


data Matrix = Matrix 
  { _matrixX, _matrixY :: Vector
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeMatrix :: Vector -> Vector -> Matrix
makeMatrix = Matrix

matToPair :: Matrix -> (Vector,Vector)
matToPair Matrix{_matrixX=x,_matrixY=y} = (x,y)

matFromPair :: (Vector,Vector) -> Matrix
matFromPair = uncurry makeMatrix

makeFields ''Matrix

instance Each Matrix Matrix Vector Vector where
  -- each :: Traversal' Vector Double
  each inj Matrix{_matrixX=x,_matrixY=y} = Matrix <$> inj x <*> inj y

-- matAsPair :: Iso' Matrix (Vector, Vector)
matAsPair :: (Functor f, Profunctor p) => 
  p (Vector, Vector) (f (Vector, Vector)) -> p Matrix (f Matrix)
matAsPair = iso matToPair matFromPair


data Affine = Affine 
  { _affineLinear :: Matrix
  , _affineTranslation :: Vector
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeAffine :: Matrix -> Vector -> Affine
makeAffine = Affine

affToPair :: Affine -> (Matrix,Vector)
affToPair Affine{_affineLinear=x,_affineTranslation=y} = (x,y)

affFromPair :: (Matrix,Vector) -> Affine
affFromPair = uncurry makeAffine

makeFields ''Affine

-- affAsPair :: Iso' Affine (Matrix, Vector)
affAsPair :: (Functor f, Profunctor p) => 
  p (Matrix, Vector) (f (Matrix, Vector)) -> p Affine (f Affine)
affAsPair = iso affToPair affFromPair


-- line segment
data Segment = Segment
  { _segmentStart :: Point
  , _segmentEnd :: Point
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeSegment :: Point -> Point -> Segment
makeSegment = Segment

segToPair :: Segment -> (Point,Point)
segToPair Segment{_segmentStart=x,_segmentEnd=y} = (x,y)

segFromPair :: (Point,Point) -> Segment
segFromPair = uncurry makeSegment

makeFields ''Segment

-- segAsPair :: Iso' Segment (Point, Point)
segAsPair :: (Functor f, Profunctor p) => 
  p (Point, Point) (f (Point, Point)) -> p Segment (f Segment)
segAsPair = iso segToPair segFromPair


-- open polygon
newtype PolyLine = PolyLine (Array Integer Point)
  deriving (Show, Read, Eq, Ord, Generic) 
makePolyLine :: [Point] -> PolyLine
makePolyLine ps = PolyLine $ listArray (0,fromIntegral $ length ps - 1) ps

plFromArray :: (Array Integer Point) -> PolyLine
plFromArray arr = PolyLine arr

plToArray :: PolyLine -> Array Integer Point
plToArray (PolyLine arr) = arr

plToPoints :: PolyLine -> [Point]
plToPoints (PolyLine arr) = elems arr

plAsPoints :: Iso' PolyLine [Point]
plAsPoints = iso plToPoints makePolyLine

plAsArray :: Iso' PolyLine (Array Integer Point)
plAsArray = iso plToArray plFromArray

segments :: Fold PolyLine Segment
segments = folding $ \pl -> 
  let
    pts = plToPoints pl
  in
    map (uncurry makeSegment) $ zip pts (tail pts)




data Circle = Circle 
  { _circleCenter :: Point
  , _circleRadius :: Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeCircle :: Point -> Double -> Circle
makeCircle = Circle

makeFields ''Circle

circToPair :: Circle -> (Point, Double)
circToPair c = (c ^. center, c^. radius)

circFromPair :: (Point,Double) -> Circle
circFromPair = uncurry makeCircle

circAsPair :: Iso' Circle (Point, Double)
circAsPair = iso circToPair circFromPair


data HalfPlane = HalfPlane
  { _halfPlaneNormal :: Vector
  , _halfPlaneRadius :: Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeFields ''HalfPlane

makeHalfPlane :: Vector -> Double -> HalfPlane
makeHalfPlane = HalfPlane

hpToPair :: HalfPlane -> (Vector, Double)
hpToPair hp = (hp ^. normal, hp ^. radius)

hpFromPair :: (Vector, Double) -> HalfPlane
hpFromPair = uncurry makeHalfPlane

hpAsPair :: Iso' HalfPlane (Vector, Double)
hpAsPair = iso hpToPair hpFromPair


newtype ConvexPolytope = ConvexPolytope [HalfPlane]
  deriving (Show, Read, Eq, Ord, Generic)

makeConvexPolytope :: [HalfPlane] -> ConvexPolytope
makeConvexPolytope = ConvexPolytope

-- closed polygon
-- is a polyline whose first and last points are the same
data Polygon = Polygon 
  { _polygonBoundary :: PolyLine 
  , _polygonRegion :: Maybe ConvexPolytope
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeFields ''Polygon

makePolygon :: PolyLine -> Maybe ConvexPolytope -> Polygon
makePolygon = Polygon

newtype ImplicitRegion = ImplicitRegion (Point -> Bool) 
  deriving (Generic)

