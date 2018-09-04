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

import Polynomial

import Data.Maybe (fromJust)
import Data.List (nub)

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

data SplinePoint 
  = C1 !Double !Double -- ^ value of the function and the derivative at the point
  | C0 !Double !Double !Double -- ^ value of the function at the point,
                               --   left derivative at the point, right derivative at the point
  | NC !Double !Double !Double !Double -- ^ value of the function at the left,
                                       --   value of the function at the right,
                                       --   left derivative,
                                       --   right derivative
  deriving (Eq,Ord,Read,Show)

leftValue :: SplinePoint -> Double
leftValue (C1 val _) = val
leftValue (C0 val _ _) = val
leftValue (NC val _ _ _) = val

rightValue :: SplinePoint -> Double
rightValue (C1 val _) = val
rightValue (C0 val _ _) = val
rightValue (NC _ val _ _) = val

leftDerivative :: SplinePoint -> Double
leftDerivative (C1 _ val) = val
leftDerivative (C0 _ val _) = val
leftDerivative (NC _ _ val _) = val

rightDerivative :: SplinePoint -> Double
rightDerivative (C1 _ val) = val
rightDerivative (C0 _ _ val) = val
rightDerivative (NC _ _ _ val) = val

-- | maps a 'C1' point to a 'C0' point, and 'C0' point to a 'NC' point, 
--   and is the identity on 'NC' points
decreaseContinuity :: SplinePoint -> SplinePoint
decreaseContinuity (C1 val der) = C0 val der der
decreaseContinuity (C0 val lder rder) = NC val val lder rder
decreaseContinuity pt = pt

{-
data BoundaryPolicy
  = Clamp
  | ExtendLinear
-}

data SplineSegment

data HSpline = HSpline
  { knots :: Vector (Double,SplinePoint)
  , bounds :: !(Double,Double)
  --, boundaryPolicy :: !BoundaryPolicy
  , preF :: !LinPoly
  , postF :: !LinPoly
  , intFs :: Vector (CubPoly)
  }
  deriving (Eq,Ord,Show,Read)

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

-- ugh more polynomial bases :/
-- basis: 
-- ax^3 + bx^2 + 1 -- a+b+1=0, 3a+2b = 0 => a = 2, b=-3
-- a(x-1)^3+b(x-1)^2 + 1 -- -a + b + 1 = 0, 3a-2b = 0 => 2a-2b=2, a = -2, b=-3
buildSplineSeg :: (Double,SplinePoint) -> (Double,SplinePoint) -> CubPoly
buildSplineSeg (t0,sp0) (t1,sp1) =
  let
    rat = t1-t0
    lv = rightValue sp0
    ld = (rightDerivative sp0)*rat
    rv = leftValue sp1
    rd = (leftDerivative sp1)*rat
  in
    composeCubLin (2*lv-2*rv+ld+rd,-3*lv+3*rv-2*ld-rd,ld,lv) (1/rat,-t0/rat)

-- build a linear poly given a point on its graph and its slope
-- y-y0=m(t-t0)
buildLinSeg :: (Double,Double) -> Double -> LinPoly
buildLinSeg (t0,y0) m = (m,y0-m*t0)

-- knots must be sorted by time and nonempty
buildHSpline :: Vector (Double,SplinePoint) -> HSpline
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
evaluateHSpline :: HSpline -> Double -> Double
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

interpolateDifferentiable :: (Double -> Double, Double -> Double) -> Vector Double -> HSpline
interpolateDifferentiable (f,fp) ts = buildHSpline $ V.zip ts (V.zipWith C1 (fmap f ts) (fmap fp ts))

interpolateDifferentiableEvenly :: (Double -> Double, Double -> Double) -> (Double,Double) -> Double -> HSpline
interpolateDifferentiableEvenly fs (s,e) st = 
  let
    lpt = (floor ((e-s)/st)) :: Int 
    ts = V.fromList (nub $ (fmap ((*st) . fromIntegral) [0..lpt]) ++ [e])
  in
    interpolateDifferentiable fs ts

