{-# LANGUAGE UndecidableInstances #-}
-- needed for the instances of HasX and HasY below, which are safe, but don't satisfy the Coverage condition

--
-- Types.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Types where
  --( 
  --) where

import GHC.Generics (Generic)

-- Lens imports
import Control.Lens
import Control.Lens.TH
import Control.Lens.Each
import Control.Lens.Fold (folding)
import Control.Lens.Iso (iso)

import Paired

--Lens version of Point
data Point = Point 
  { _pointX, _pointY :: !Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeFields ''Point

makePoint :: Double -> Double -> Point
makePoint = Point

ptToPair :: Point -> (Double,Double)
ptToPair Point{_pointX=x,_pointY=y} = (x,y)

ptFromPair :: (Double,Double) -> Point
ptFromPair = uncurry makePoint

instance Paired Point where
  type TLf Point = Double
  type TRt Point = Double
  toPair = ptToPair
  fromPair = ptFromPair

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
  { _vectorX, _vectorY :: !Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeVector :: Double -> Double -> Vector
makeVector = Vector

vecToPair :: Vector -> (Double,Double)
vecToPair Vector{_vectorX=x,_vectorY=y} = (x,y)

vecFromPair :: (Double,Double) -> Vector
vecFromPair = uncurry makeVector

instance Paired Vector where
  type TLf Vector = Double
  type TRt Vector = Double
  toPair = vecToPair
  fromPair = vecFromPair


makeFields ''Vector

instance Each Vector Vector Double Double where
  -- each :: Traversal' Vector Double
  each inj Vector{_vectorX=x,_vectorY=y} = Vector <$> inj x <*> inj y

-- vecAsPair :: Iso' Vector (Double, Double)
vecAsPair :: (Functor f, Profunctor p) => 
  p (Double, Double) (f (Double, Double)) -> p Vector (f Vector)
vecAsPair = iso vecToPair vecFromPair


newtype Covector = Covector { _covectorDual :: Vector }

makeCovector :: Double -> Double -> Covector
makeCovector x y = Covector (makeVector x y)

covecToPair :: Covector -> (Double, Double)
covecToPair = vecToPair . _covectorDual

covecFromPair :: (Double,Double) -> Covector
covecFromPair = uncurry makeCovector

instance Paired Covector where
  type TLf Covector = Double
  type TRt Covector = Double
  toPair = covecToPair
  fromPair = covecFromPair


makeFields ''Covector

instance HasX Covector Double where
  x = dual . x
instance HasY Covector Double where
  y = dual . y

instance Each Covector Covector Double Double where
  -- each :: Traversal' Vector Double
  each inj Covector{_covectorDual=Vector{_vectorX=x,_vectorY=y}} = makeCovector <$> inj x <*> inj y

covecAsPair :: Iso' Covector (Double, Double)
covecAsPair = iso covecToPair covecFromPair

dualize :: Iso' Vector Covector
dualize = iso Covector _covectorDual

data Matrix = Matrix 
  { _matrixX, _matrixY :: !Vector
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeMatrix :: Vector -> Vector -> Matrix
makeMatrix = Matrix

matToPair :: Matrix -> (Vector,Vector)
matToPair Matrix{_matrixX=x,_matrixY=y} = (x,y)

matFromPair :: (Vector,Vector) -> Matrix
matFromPair = uncurry makeMatrix

instance Paired Matrix where
  type TLf Matrix = Vector
  type TRt Matrix = Vector
  toPair = matToPair
  fromPair = matFromPair

-- matAsPair :: Iso' Matrix (Vector, Vector)
matAsPair :: (Functor f, Profunctor p) => 
  p (Vector, Vector) (f (Vector, Vector)) -> p Matrix (f Matrix)
matAsPair = iso matToPair matFromPair

-- pair of columns
matToComponents :: Matrix -> ((Double, Double), (Double, Double))
matToComponents mat = mat^.matAsPair & each %~ vecToPair

matFromComponents :: ((Double,Double), (Double, Double)) -> Matrix
matFromComponents pair = (pair & each %~ vecFromPair)^.from matAsPair

-- matAsPair :: Iso' Matrix (Vector, Vector)
matAsComponents :: Iso' Matrix ((Double, Double), (Double, Double))
matAsComponents = iso matToComponents matFromComponents

makeFields ''Matrix

instance Each Matrix Matrix Vector Vector where
  -- each :: Traversal' Vector Double
  each inj Matrix{_matrixX=x,_matrixY=y} = Matrix <$> inj x <*> inj y



data Affine = Affine 
  { _affineLinear :: !Matrix
  , _affineTranslation :: !Vector
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeAffine :: Matrix -> Vector -> Affine
makeAffine = Affine

affToPair :: Affine -> (Matrix,Vector)
affToPair Affine{_affineLinear=x,_affineTranslation=y} = (x,y)

affFromPair :: (Matrix,Vector) -> Affine
affFromPair = uncurry makeAffine

instance Paired Affine where
  type TLf Affine = Matrix
  type TRt Affine = Vector
  toPair = affToPair
  fromPair = affFromPair

makeFields ''Affine

-- affAsPair :: Iso' Affine (Matrix, Vector)
affAsPair :: (Functor f, Profunctor p) => 
  p (Matrix, Vector) (f (Matrix, Vector)) -> p Affine (f Affine)
affAsPair = iso affToPair affFromPair


-- line segment
data Segment = Segment
  { _segmentStart :: !Point
  , _segmentEnd :: !Point
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeSegment :: Point -> Point -> Segment
makeSegment = Segment

segToPair :: Segment -> (Point,Point)
segToPair Segment{_segmentStart=x,_segmentEnd=y} = (x,y)

segFromPair :: (Point,Point) -> Segment
segFromPair = uncurry makeSegment

instance Paired Segment where
  type TLf Segment = Point
  type TRt Segment = Point
  toPair = segToPair
  fromPair = segFromPair

makeFields ''Segment

-- segAsPair :: Iso' Segment (Point, Point)
segAsPair :: (Functor f, Profunctor p) => 
  p (Point, Point) (f (Point, Point)) -> p Segment (f Segment)
segAsPair = iso segToPair segFromPair


instance Each Segment Segment Point Point where
  -- each :: Traversal' Point Double
  -- Applicative f => (Double -> f Double) -> Point -> f Point
  each inj Segment{_segmentStart=s,_segmentEnd=e} = Segment <$> inj s <*> inj e


data Circle = Circle 
  { _circleCenter :: !Point
  , _circleRadius :: !Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeCircle :: Point -> Double -> Circle
makeCircle = Circle

makeFields ''Circle

circToPair :: Circle -> (Point, Double)
circToPair c = (c ^. center, c^. radius)

circFromPair :: (Point,Double) -> Circle
circFromPair = uncurry makeCircle

instance Paired Circle where
  type TLf Circle = Point
  type TRt Circle = Double
  toPair = circToPair
  fromPair = circFromPair

circAsPair :: Iso' Circle (Point, Double)
circAsPair = iso circToPair circFromPair

{-
data Conic = Conic
  { _conicCenter :: !Point
  , _conicMatrix :: !Matrix
  --, _conicThreshold :: !Double -- might give more accurate close to degenerate conics. do I need close to degenerate conics
  }

makeConic :: Point -> Matrix -> Conic
makeConic = Conic

makeFields ''Conic

conicToPair :: Conic -> (Point, Matrix)
conicToPair c = (c ^. center, c ^. matrix)

conicFromPair :: (Point,Matrix) -> Conic
conicFromPair = uncurry makeConic

conicAsPair :: Iso' Conic (Point, Matrix)
conicAsPair = iso conicToPair conicFromPair

discriminant :: Conic -> Double 
discriminant

_Circle :: Prism' Conic Circle
-}

data HalfPlane = HalfPlane
  { _halfPlaneNormal :: !Vector
  , _halfPlaneRadius :: !Double
  }
  deriving (Show, Read, Eq, Ord, Generic)

makeFields ''HalfPlane

makeHalfPlane :: Vector -> Double -> HalfPlane
makeHalfPlane = HalfPlane

hpToPair :: HalfPlane -> (Vector, Double)
hpToPair hp = (hp ^. normal, hp ^. radius)

hpFromPair :: (Vector, Double) -> HalfPlane
hpFromPair = uncurry makeHalfPlane

instance Paired HalfPlane where
  type TLf HalfPlane = Vector
  type TRt HalfPlane = Double
  toPair = hpToPair
  fromPair = hpFromPair

hpAsPair :: Iso' HalfPlane (Vector, Double)
hpAsPair = iso hpToPair hpFromPair


newtype ConvexPolytope = ConvexPolytope [HalfPlane]
  deriving (Show, Read, Eq, Ord, Generic)

makeConvexPolytope :: [HalfPlane] -> ConvexPolytope
makeConvexPolytope = ConvexPolytope

convtopeToHPlanes :: ConvexPolytope -> [HalfPlane]
convtopeToHPlanes (ConvexPolytope hps) = hps

convtopeAsHPlanes :: Iso' ConvexPolytope [HalfPlane]
convtopeAsHPlanes = iso convtopeToHPlanes makeConvexPolytope

-- forall f. Applicative f => (HalfPlane -> f HalfPlane) -> ConvexPolytope -> f ConvexPolytope
hplanes :: Traversal' ConvexPolytope HalfPlane
hplanes inj = fmap makeConvexPolytope . traverse inj . convtopeToHPlanes


data Box = Box
  { _boxCorner :: !Point -- lower left corner
  , _boxDimensions :: !Vector
  }
  deriving (Show, Read, Eq, Ord, Generic)


makeBox :: Point -> Vector -> Box
makeBox = Box

makeSquare :: Point -> Double -> Box
makeSquare pt s = makeBox pt ((s,s)^.from vecAsPair)

makeFields ''Box

boxLeft :: Box -> Double
boxLeft px = px^.corner.x

boxRight :: Box -> Double
boxRight px = px^.corner.x + px^.dimensions.x

boxBottom :: Box -> Double
boxBottom px = px^.corner.y

boxTop :: Box -> Double 
boxTop px = px^.corner.y + px^.dimensions.y

class GBounded a where
  bounds :: a -> Box

instance (HasX a c, HasX b d) => HasX (a,b) (c,d) where
  -- x :: Functor f => ((c,d) -> f (c,d)) -> (a,b) -> f (a,b)
  x f (v1,v2) = fmap (\(cr,dr) -> (v1 & x .~ cr, v2 & x .~ dr)) (f (v1^.x,v2^.x))
instance (HasX a d, HasX b e, HasX c f) => HasX (a,b,c) (d,e,f) where
  -- x :: Functor f => ((c,d) -> f (c,d)) -> (a,b) -> f (a,b)
  x f (v1,v2,v3) = fmap (\(dr,er,fr) -> (v1 & x .~ dr, v2 & x .~ er,v3 & x .~ fr)) (f (v1^.x,v2^.x,v3^.x))
instance (HasX a e, HasX b f, HasX c g, HasX d h) => HasX (a,b,c,d) (e,f,g,h) where
  -- x :: Functor f => ((c,d) -> f (c,d)) -> (a,b) -> f (a,b)
  x f (v1,v2,v3,v4) =
    fmap
      (\(er,fr,gr,hr) -> (v1 & x .~ er, v2 & x .~ fr,v3 & x .~ gr,v4 & x .~ hr))
      (f (v1^.x,v2^.x,v3^.x,v4^.x))
instance (HasY a c, HasY b d) => HasY (a,b) (c,d) where
  -- y :: Functor f => ((c,d) -> f (c,d)) -> (a,b) -> f (a,b)
  y f (v1,v2) = fmap (\(cr,dr) -> (v1 & y .~ cr, v2 & y .~ dr)) (f (v1^.y,v2^.y))
instance (HasY a d, HasY b e, HasY c f) => HasY (a,b,c) (d,e,f) where
  -- y :: Functor f => ((c,d) -> f (c,d)) -> (a,b) -> f (a,b)
  y f (v1,v2,v3) = fmap (\(dr,er,fr) -> (v1 & y .~ dr, v2 & y .~ er,v3 & y .~ fr)) (f (v1^.y,v2^.y,v3^.y))
instance (HasY a e, HasY b f, HasY c g, HasY d h) => HasY (a,b,c,d) (e,f,g,h) where
  -- y :: Functor f => ((c,d) -> f (c,d)) -> (a,b) -> f (a,b)
  y f (v1,v2,v3,v4) =
    fmap
      (\(er,fr,gr,hr) -> (v1 & y .~ er, v2 & y .~ fr,v3 & y .~ gr,v4 & y .~ hr))
      (f (v1^.y,v2^.y,v3^.y,v4^.y))

