{-# LANGUAGE UndecidableInstances #-}
--
-- Spline.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Spline
  ( module Spline
  ) where

-- I need a generalized cubic spline, and ways to produce and evaluate them.
-- Shoud handle 1D case (producing C1 splines, tangents (er derivatives) are equal across knots) as well as 
-- 2D case (producing G1 splines, tangents are parallel across knots) also C1 splines are sometimes necessary
-- for 2D case as well, but much less often.

-- desired supported interpolation methods:
-- Catmull/Rom (Uniform,Centripetal,Chordal) see wiki (2D/C1)
-- Monotone cubic interpolation (1D)
-- Finite Difference (2D/C1) (Maybe do weighted finite difference to produce G1 curve?)
-- Cardinal spline 
-- kochanek-Bartels spline (2D, continuity varies)

-- would be good to have efficient 4D matrices/vectors for this.
-- Look into SIMD
-- also uhhhh maybe we should be using Float instead of Double?
-- Anyway, that's super low priority. First a good interface is necessary.
-- Less low priority is of course the actual 4d matrices/vectors. Those are crucial to improve.
-- Hm. Maybe switch to the linear library by edward kmett?

import MathClasses
import Polynomial

import Data.Maybe (fromJust)
import Data.List (nub)

import Data.Vector (Vector, (!))
import qualified Data.Vector as V


data SplinePointG a b
  = C1 !b !a -- ^ value of the function and the derivative at the point
  | C0 !b !a !a -- ^ value of the function at the point,
                               --   left derivative at the point, right derivative at the point
  | NC !b !b !a !a -- ^ value of the function at the left,
                                       --   value of the function at the right,
                                       --   left derivative,
                                       --   right derivative
  deriving (Eq,Ord,Read,Show)

-- | assumes (tho it isn't essential that it is) that the tangent is normalized to unit length
mkG1 :: Vectorlike a => b -> a -> (Double,Double) -> SplinePointG a b
mkG1 v nTang (lscale,rscale) = C0 v (lscale*.nTang) (rscale*.nTang)

type SplinePoint = SplinePointG Double Double

leftValue :: SplinePointG a b -> b
leftValue (C1 val _) = val
leftValue (C0 val _ _) = val
leftValue (NC val _ _ _) = val

rightValue :: SplinePointG a b -> b
rightValue (C1 val _) = val
rightValue (C0 val _ _) = val
rightValue (NC _ val _ _) = val

leftDerivative :: SplinePointG a b -> a
leftDerivative (C1 _ val) = val
leftDerivative (C0 _ val _) = val
leftDerivative (NC _ _ val _) = val

rightDerivative :: SplinePointG a b -> a
rightDerivative (C1 _ val) = val
rightDerivative (C0 _ _ val) = val
rightDerivative (NC _ _ _ val) = val

-- | maps a 'C1' point to a 'C0' point, and 'C0' point to a 'NC' point, 
--   and is the identity on 'NC' points
decreaseContinuity :: SplinePointG a b -> SplinePointG a b
decreaseContinuity (C1 val der) = C0 val der der
decreaseContinuity (C0 val lder rder) = NC val val lder rder
decreaseContinuity pt = pt

{-
data BoundaryPolicy
  = Clamp
  | ExtendLinear
-}

type HSpline = HSplineG Double Double

data HSplineG a b = HSpline
  { knots :: Vector (Double,SplinePointG a b)
  , bounds :: !(Double,Double)
  --, boundaryPolicy :: !BoundaryPolicy
  , preF :: !(LinearPoly a b)
  , postF :: !(LinearPoly a b)
  , intFs :: Vector (CubicPoly a b)
  }
  deriving (Eq,Ord,Show,Read)

{-
-- derivative is (6,-6,0)
hsBasisZ :: CubPoly
hsBasisZ = (2,-3,0,1)

-- derivative is (-6,6,0)
hsBasisO :: CubPoly
hsBasisO = (-2,3,0,0)

-- derivative is (3,-4,1)
hsBasisZP :: CubPoly
hsBasisZP = (1,-2,1,0)

-- derivative is (3,-2,0)
hsBasisOP :: CubPoly
hsBasisOP = (1,-1,0,0)
-}

-- ugh more polynomial bases :/
-- basis: 
-- ax^3 + bx^2 + 1 -- a+b+1=0, 3a+2b = 0 => a = 2, b=-3
-- a(x-1)^3+b(x-1)^2 + 1 -- -a + b + 1 = 0, 3a-2b = 0 => 2a-2b=2, a = -2, b=-3
buildSplineSeg :: Polynomializable a b =>
  (Double,SplinePointG a b) -> (Double,SplinePointG a b) -> CubicPoly a b
buildSplineSeg (t0,sp0) (t1,sp1) =
  let
    rat = t1-t0
    lv = rightValue sp0
    ld = rat*.(rightDerivative sp0)
    rv = leftValue sp1
    rd = rat*.(leftDerivative sp1)
  in
    composeCubLin 
      ( (2::Double)*.(lv-.rv)+.ld+.rd
      , (3::Double)*.(rv-.lv) -. (2::Double)*.ld -. rd
      , ld
      , lv
      )
      (1/rat,-t0/rat)

-- build a linear poly given a point on its graph and its slope
-- y-y0=m(t-t0)
buildLinSeg :: Polynomializable a b => (Double,b) -> a -> LinearPoly a b
buildLinSeg (t0,y0) m = (m, y0 -. t0*.m)

-- knots must be sorted by time and nonempty
buildHSpline :: Polynomializable a b => Vector (Double,SplinePointG a b) -> HSplineG a b
buildHSpline vec =
  let
    (ft,fp) = V.head vec
    (lt,lp) = V.last vec
    bnds = (ft,lt)
    prdKs = V.zip vec (V.tail vec)
    ints = fmap (uncurry buildSplineSeg) prdKs
  in
    HSpline
      vec
      bnds
      (buildLinSeg (ft,leftValue fp) (leftDerivative fp))
      (buildLinSeg (lt,rightValue lp) (rightDerivative lp))
      ints



-- all intervals are half open with open side on the right
evaluateHSpline :: Polynomializable a b => HSplineG a b -> Double -> b
evaluateHSpline hspline t = 
  let
    (l,r) = bounds hspline
  in
    if t < l
    then
      evalLinear (preF hspline) t
    else
      if t >= r
      then
        evalLinear (postF hspline) t
      else
        -- should binary search here TODO, see gallopingSearch in vector algorithms
        let 
          -- this is safe because of the previous checks
          i = fromJust $ V.findIndex ((> t) . fst) $ knots hspline
        in
          evalCubic ((intFs hspline)!(i-1)) t

instance (Polynomializable a b) => EvaluatableClass (HSplineG a b) where
  type Domain (HSplineG a b) c = (c~Double)
  type Codomain (HSplineG a b) c = b
  evaluate = evaluateHSpline

-- | probably not useful for two dimensional functions
interpolateDifferentiable :: Polynomializable a b => 
  (Double -> b, Double -> a) -> Vector Double -> HSplineG a b
interpolateDifferentiable (f,fp) ts = buildHSpline $ V.zip ts (V.zipWith C1 (fmap f ts) (fmap fp ts))

interpolateDifferentiableEvenly :: Polynomializable a b => 
  (Double -> b, Double -> a) -> (Double,Double) -> Double -> HSplineG a b
interpolateDifferentiableEvenly fs (s,e) st = 
  let
    lpt = (floor ((e-s)/st)) :: Int 
    ts = V.fromList (nub $ (fmap ((*st) . fromIntegral) [0..lpt]) ++ [e])
  in
    interpolateDifferentiable fs ts


--buildAntialiasingSpline :: Double -> [(Double,Double)] -> HSpline
--buildAntialiasingSpline 


