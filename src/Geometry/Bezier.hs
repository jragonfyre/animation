--
-- Bezier.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Bezier where

import Geometry.Types
import Geometry.Curve.Types
import Geometry.Region
import Geometry.Affine

import Control.Lens ((^.), from, (&), (.~), over, _1)

import Utils

import Polynomial

data Bezier2 = Bezier2 
  { start2 :: Point
  , control2 :: Point
  , end2 :: Point
  , xCoeffs2 :: (Double, Double, Double)
  , yCoeffs2 :: (Double, Double, Double)
  , implicitParams2 :: (Double,Double,Double,Double,Double,Double)
  , boundingBox2 :: Box
  }
  deriving (Read,Show,Eq,Ord)

-- p1 p2 p3 -> quadratic polynomial
computeBez2Coeffs :: (Double,Double,Double) -> (Double,Double,Double)
computeBez2Coeffs (s,c,e) = (e-2*c+s,-2*s+2*c,s)

computeBez3Coeffs :: (Double,Double,Double,Double) -> (Double,Double,Double,Double)
computeBez3Coeffs (s,c,d,e) = (-s+3*c-3*d+e,3*s-6*c+3*d,-3*s+3*c,s)

-- | Produces a 'Bezier2' from coefficients for @x@ and @y@
bezierFromCoeffs2 :: QuadPoly -> QuadPoly -> Bezier2
bezierFromCoeffs2 xs ys = 
  let
    (sx,cx,ex) = stdToBezBasis2 xs
    (sy,cy,ey) = stdToBezBasis2 ys
  in
    makeBezier2 (makePoint sx sy) (makePoint cx cy) (makePoint ex ey)

-- the x coefficients must be for xh
-- | Produces an 'RBezier2' from coefficients for homogeneous @x@, homogeneous @y@, and @w@.
rbezierFromCoeffs2 :: QuadPoly -> QuadPoly -> QuadPoly -> RBezier2
rbezierFromCoeffs2 xhs yhs ws = 
  let
    (sxh,cxh,exh) = stdToBezBasis2 xhs
    (syh,cyh,eyh) = stdToBezBasis2 yhs
    (sw,cw,ew) = stdToBezBasis2 ws
  in
    makeRBezier2
      (makePoint (sxh/sw) (syh/sw),sw)
      (makePoint (cxh/cw) (cyh/cw),cw)
      (makePoint (exh/ew) (eyh/ew),ew)

-- | Produces a 'Bezier3' from coefficients for @x@ and @y@
bezierFromCoeffs3 :: CubPoly -> CubPoly -> Bezier3
bezierFromCoeffs3 xs ys = 
  let
    (sx,cx,dx,ex) = stdToBezBasis3 xs
    (sy,cy,dy,ey) = stdToBezBasis3 ys
  in
    makeBezier3 (makePoint sx sy) (makePoint cx cy) (makePoint dx dy) (makePoint ex ey)

-- | Produces an 'RBezier3' from coefficients for homogeneous @x@, homogeneous @y@, and @w@.
rbezierFromCoeffs3 :: CubPoly -> CubPoly -> CubPoly -> RBezier3
rbezierFromCoeffs3 xhs yhs ws = 
  let
    (sxh,cxh,dxh,exh) = stdToBezBasis3 xhs
    (syh,cyh,dyh,eyh) = stdToBezBasis3 yhs
    (sw,cw,dw,ew) = stdToBezBasis3 ws
  in
    makeRBezier3
      (makePoint (sxh/sw) (syh/sw),sw)
      (makePoint (cxh/cw) (cyh/cw),cw)
      (makePoint (dxh/dw) (dyh/dw),dw)
      (makePoint (exh/ew) (eyh/ew),ew)

-- | Restricts (or even expands) a quadratic Bezier to a particular interval of parameter values
restrictTo2 :: (Double,Double) -- ^ @(intervalStart,intervalEnd)@
            -> Bezier2 -> Bezier2
restrictTo2 (st,ed) bez = 
  let
    l = (ed-st,st)
  in
    bezierFromCoeffs2 (composeQuadLin (xCoeffs2 bez) l) (composeQuadLin (yCoeffs2 bez) l)

-- | Restricts (or even expands) a cubic Bezier to a particular interval of parameter values
restrictTo3 :: (Double,Double) -- ^ @(intervalStart,intervalEnd)@
            -> Bezier3 -> Bezier3
restrictTo3 (st,ed) bez = 
  let
    l = (ed-st,st)
  in
    bezierFromCoeffs3 (composeCubLin (xCoeffs3 bez) l) (composeCubLin (yCoeffs3 bez) l)

-- | Restricts (or even expands) a rational quadratic Bezier to a particular interval of parameter values
restrictToR2 :: (Double,Double) -- ^ @(intervalStart,intervalEnd)@
             -> RBezier2
             -> RBezier2
restrictToR2 (st,ed) rbez = 
  let
    l = (ed-st,st)
  in
    rbezierFromCoeffs2
      (composeQuadLin (rxCoeffs2 rbez) l)
      (composeQuadLin (ryCoeffs2 rbez) l)
      (composeQuadLin (rzCoeffs2 rbez) l)

-- | Restricts (or even expands) a rational cubic Bezier to a particular interval of parameter values
restrictToR3 :: (Double,Double) -- ^ @(intervalStart,intervalEnd)@
             -> RBezier3 
             -> RBezier3
restrictToR3 (st,ed) rbez = 
  let
    l = (ed-st,st)
  in
    rbezierFromCoeffs3
      (composeCubLin (rxCoeffs3 rbez) l)
      (composeCubLin (ryCoeffs3 rbez) l)
      (composeCubLin (rzCoeffs3 rbez) l)

-- for laziness reasons, (in the nontechnical sense), I'm going to implement this in terms of restrictTo3 
-- rather than appropriately computing all the points, though that (done well)
-- is almost certainly more efficient, since
-- there is probably a lot of redundant computing going on at internal subdivision points and control points
-- around internal subdivision points, which have restrictions placed on them by continuity and smoothness.
-- TODO: Take advantage of restrictions to make this more efficient
-- assumes the list of doubles is sorted in increasing order and contains 0 and 1
-- | Subdivides a 'Bezier3' into a list of 'Bezier3's at the points provided.
--
--   __Requires:__ Zero and one must be in the list, and it must be sorted in increasing order.
subdivideBezier3 :: [Double] -- ^ the points to subdivide the Bezier at
                 -> Bezier3 -- ^ the cubic Bezier to subdivide
                 -> [Bezier3] -- ^ the subdivided curves
subdivideBezier3 ks bez = map (flip restrictTo3 bez) $ zip ks (tail ks)

-- | Subdivides a 'Bezier2' into a list of 'Bezier2's at the points provided.
--
--   __Requires:__ Zero and one must be in the list, and it must be sorted in increasing order.
subdivideBezier2 :: [Double]  -- ^ the points to subdivide the Bezier at
                 -> Bezier2 -- ^ the quadratic Bezier to subdivide
                 -> [Bezier2] -- ^ the subdivided curves
subdivideBezier2 ks bez = map (flip restrictTo2 bez) $ zip ks (tail ks)

-- | Subdivides a 'RBezier3' into a list of 'RBezier3's at the points provided.
--
--   __Requires:__ Zero and one must be in the list, and it must be sorted in increasing order.
subdivideRBezier3 :: [Double] -- ^ the points to subdivide the rational Bezier at
                  -> RBezier3 -- ^ the cubic rational Bezier to subdivide
                  -> [RBezier3] -- ^ the subdivided curves
subdivideRBezier3 ks rbez = map (flip restrictToR3 rbez) $ zip ks (tail ks)

-- | Subdivides a 'RBezier2' into a list of 'RBezier2's at the points provided.
--
--   __Requires:__ Zero and one must be in the list, and it must be sorted in increasing order.
subdivideRBezier2 :: [Double]  -- ^ the points to subdivide the rational Bezier at
                  -> RBezier2 -- ^ the quadratic rational Bezier to subdivide
                  -> [RBezier2] -- ^ the subdivided curves
subdivideRBezier2 ks rbez = map (flip restrictToR2 rbez) $ zip ks (tail ks)

-- | Type synonym representing a weighted point. Alternatively, if this is the point @((x,y),w)@, this 
--   represents the point in projective space with homogeneous coordinates @[xw:yw:w]@.
type WPoint = (Point,Double)

-- | Correctly transform weighted points by affine transformations. Equivalent to transforming the
--   corresponding projective
--   point by the corresponding projective transformation.
--
--   Might be better to remove the instance @('Geometric' a, 'Functor' f) => 'Geometric' (f a)@, and
--   make 'WPoint' an instance of 'Geometric', but this is under consideration. TODO.
transformWPoint :: Affine -> WPoint -> WPoint
transformWPoint aff = over _1 (transform aff)

{-
instance Geometric (Point,Double) where
  -- not sure if this is a good definition
  transform aff (pt,w) = (transform aff pt,w)
-}

-- | A rational quadratic Bezier. TODO: expand documentation
data RBezier2 = RBezier2
  { rstart2 :: WPoint -- point and weight (NOT HOMOGENEOUS COORDINATES!)
  , rcontrol2 :: WPoint
  , rend2 :: WPoint
  , rxCoeffs2 :: (Double,Double,Double) 
    -- TODO rename xhCoeffs to make it clear that these are the coefficients of the homogeneous x coordinate.
  , ryCoeffs2 :: (Double,Double,Double)
  , rzCoeffs2 :: (Double,Double,Double)
  , rboundingBox2 :: Box -- this is only valid when the weights are positive
  }
  deriving (Read,Show,Eq,Ord)

instance GBounded Bezier2 where
  bounds = boundingBox2

instance Geometric Bezier2 where
  transform aff Bezier2{start2=s,control2=c,end2=e} =
    makeBezier2
      (transform aff s)
      (transform aff c)
      (transform aff e)

instance GBounded RBezier2 where
  bounds = rboundingBox2

instance Geometric RBezier2 where
  transform aff RBezier2{rstart2=(s,sw),rcontrol2=(c,cw),rend2=(e,ew)} = 
    makeRBezier2
      (transform aff s,sw)
      (transform aff c,cw)
      (transform aff e,ew)

parametrization2 :: Bezier2 -> Double -> Point
parametrization2 bez t =
  let 
    (a2,a1,a0) = xCoeffs2 bez
    (b2,b1,b0) = yCoeffs2 bez
  in
    makePoint
      (t*(t*a2+a1)+a0)
      (t*(t*b2+b1)+b0)

rparametrization2 :: RBezier2 -> Double -> Point
rparametrization2 rbez t =
  let 
    xs = rxCoeffs2 rbez
    ys = ryCoeffs2 rbez
    zs = rzCoeffs2 rbez
    x = evalQuadratic xs t
    y = evalQuadratic ys t
    z = evalQuadratic zs t
  in
    makePoint
      (x/z)
      (y/z)

rderivative2 :: RBezier2 -> Double -> Vector
rderivative2 rbez t = 
  let
    xs = rxCoeffs2 rbez
    xps = derivCoeffsQuadratic xs
    ys = ryCoeffs2 rbez
    yps = derivCoeffsQuadratic ys
    zs = rzCoeffs2 rbez
    zps = derivCoeffsQuadratic zs
    x = evalQuadratic xs t
    xp = evalLinear xps t
    y = evalQuadratic ys t
    yp = evalLinear yps t
    z = evalQuadratic zs t
    zp = evalLinear zps t
  in
    makeVector
      ((xp/z)-(x*zp)/z^2)
      ((yp/z)-(y*zp)/z^2)

implicitization2 :: Bezier2 -> Point -> Double
implicitization2 bez pt =
  let
    (a,b,c,d,e,f) = implicitParams2 bez
    px = pt^.x
    py = pt^.y
  in
    a*px^2 + b*py^2 + c*px*py + d*px + e*py + f

makeBezier2 :: Point -> Point -> Point -> Bezier2
makeBezier2 sp cp ep =
  let
    sx = sp^.x
    sy = sp^.y
    cx = cp^.x
    cy = cp^.y
    ex = ep^.x
    ey = ep^.y
    --a1 = sx-2*cx+ex
    --a2 = -2*sx+2*cx
    --b1 = sy-2*cy+ey
    --b2 = -2*sy+2*cy
    a = sy^2-4*sy*cy + 2*sy*ey + 4*cy^2 - 4*cy*ey + ey^2
    b = sx^2-4*sx*cx + 2*sx*ex + 4*cx^2 - 4*cx*ex + ex^2
    c = -2*sx*sy+4*sx*cy-2*sx*ey+4*cx*sy-8*cx*cy + 4*cx*ey-2*ex*sy + 4 * ex*cy -2*ex*ey
    d = 2*sx*sy*ey - 4*sx*cy^2 + 4*sx*cy*ey-2*sx*ey^2+4*cx*sy*cy - 8*cx*sy*ey + 4*cx*cy*ey - 2*ex*sy^2 
          + 4*ex*sy*cy + 2*ex*sy*ey - 4 *ex*cy^2
    e = 2*sy*sx*ex - 4*sy*cx^2 + 4*sy*cx*ex-2*sy*ex^2+4*cy*sx*cx - 8*cy*sx*ex + 4*cy*cx*ex - 2*ey*sx^2 
          + 4*ey*sx*cx + 2*ey*sx*ex - 4 *ey*cx^2
    f = sx^2*ey^2-4*sx*cx*cy*ey - 2*sx*ex*sy*ey + 4 * sx*ex*cy^2 + 4*cx^2*sy*ey - 4*cx*ex*sy*cy + ex^2*sy^2
    --(lx,mx) = minMaxOf [sx,cx,ex]
    --(ly,my) = minMaxOf [sy,cy,ey]
  in
    Bezier2
      { start2=sp
      , control2=cp
      , end2=ep
      , xCoeffs2 = computeBez2Coeffs (sx,cx,ex) -- (a1,a2,sx)
      , yCoeffs2 = computeBez2Coeffs (sy,cy,ey) -- (b1,b2,sy)
      , implicitParams2 = (a,b,c,d,e,f)
      , boundingBox2 = convexBoundingBox [sp,cp,ep] --makeBoxSides lx mx ly my
      }

-- assumes the weights are **POSITIVE**
-- TODO maybe implement some checks or something
makeRBezier2 :: WPoint -> WPoint -> WPoint -> RBezier2
makeRBezier2 s@(sp,sz) c@(cp,cz) e@(ep,ez) =
  let
    sx = sp^.x
    sy = sp^.y
    cx = cp^.x
    cy = cp^.y
    ex = ep^.x
    ey = ep^.y
  in
    RBezier2
      { rstart2=s
      , rcontrol2=c
      , rend2=e
      , rxCoeffs2 = computeBez2Coeffs (sx*sz,cx*cz,ex*ez)
      , ryCoeffs2 = computeBez2Coeffs (sy*sz,cy*cz,ey*ez)
      , rzCoeffs2 = computeBez2Coeffs (sz,cz,ez)
      , rboundingBox2 = convexBoundingBox [sp,cp,ep]
      }

data Bezier3 = Bezier3 
  { start3 :: Point
  , stCont3 :: Point
  , endCont3 :: Point
  , end3 :: Point
  , xCoeffs3 :: (Double,Double,Double,Double)
  , yCoeffs3 :: (Double,Double,Double,Double)
  --, implicitization3 :: Point -> Double
  , boundingBox3 :: Box
  }
  deriving (Read, Show, Eq, Ord)

makeBezier3 :: Point -> Point -> Point -> Point -> Bezier3
makeBezier3 sp cp1 cp2 ep =
  let
    sx = sp^.x
    sy = sp^.y
    cx = cp1^.x
    cy = cp1^.y
    dx = cp2^.x
    dy = cp2^.y
    ex = ep^.x
    ey = ep^.y
    {-
    a3 = -sx+3*cx-3*dx+ex
    a2 = 3*sx-6*cx+3*dx
    a1 = -3*sx+3*cx
    a0 = sx
    b3 = -sy+3*cy-3*dy+ey
    b2 = 3*sy-6*cy+3*dy
    b1 = -3*sy+3*cy
    b0 = sy
    -}
  in
    Bezier3
      { start3=sp
      , stCont3=cp1
      , endCont3=cp2
      , end3=ep
      , xCoeffs3 = computeBez3Coeffs (sx,cx,dx,ex) -- (a3,a2,a1,a0)
      , yCoeffs3 = computeBez3Coeffs (sy,cy,dy,ey) -- (b3,b2,b1,b0)
      , boundingBox3 = convexBoundingBox [sp,cp1,cp2,ep]
      }

instance GBounded Bezier3 where
  bounds = boundingBox3

instance Geometric Bezier3 where
  transform aff Bezier3{start3=s,stCont3=c,endCont3=d,end3=e} =
    makeBezier3
      (transform aff s)
      (transform aff c)
      (transform aff d)
      (transform aff e)

parametrization3 :: Bezier3 -> Double -> Point
parametrization3 bez t =
  let
    (a3,a2,a1,a0) = xCoeffs3 bez
    (b3,b2,b1,b0) = yCoeffs3 bez
  in
    makePoint
      (t*(t*(t*a3+a2)+a1)+a0)
      (t*(t*(t*b3+b2)+b1)+b0)

bezier2Curve :: Bezier2 -> Double -> Curve
bezier2Curve bez d = 
  buildCurveWithApproximation 
    (parametrization2 bez)
    (approximate (parametrization2 bez) evenApproximator d)
  & implicit .~ (Just $ implicitization2 bez)

bezier3Curve :: Bezier3 -> Double -> Curve
bezier3Curve bez d = 
  buildCurveWithApproximation 
    (parametrization3 bez)
    (approximate (parametrization3 bez) evenApproximator d)
--  & implicit .~ (Just $ implicitization3 bez)


data RBezier3 = RBezier3
  { rstart3 :: WPoint
  , rstCont3 :: WPoint
  , rendCont3 :: WPoint
  , rend3 :: WPoint
  , rxCoeffs3 :: (Double,Double,Double,Double)
  , ryCoeffs3 :: (Double,Double,Double,Double)
  , rzCoeffs3 :: (Double,Double,Double,Double)
  , rboundingBox3 :: Box
  }
  deriving (Read, Show, Eq, Ord)

instance GBounded RBezier3 where
  bounds = rboundingBox3

instance Geometric RBezier3 where
  transform aff RBezier3{rstart3=(sp,sz),rstCont3=(cp,cz),rendCont3=(dp,dz),rend3=(ep,ez)} =
    makeRBezier3
      (transform aff sp,sz)
      (transform aff cp,cz)
      (transform aff dp,dz)
      (transform aff ep,ez)

rparametrization3 :: RBezier3 -> Double -> Point
rparametrization3 rbez t =
  let
    xs = rxCoeffs3 rbez
    ys = ryCoeffs3 rbez
    zs = rzCoeffs3 rbez
    x = evalCubic xs t
    y = evalCubic ys t
    z = evalCubic zs t
  in
    makePoint
      (x/z)
      (y/z)

rderivative3 :: RBezier3 -> Double -> Vector
rderivative3 rbez t =
  let
    xs = rxCoeffs3 rbez
    xps = derivCoeffsCubic xs
    ys = ryCoeffs3 rbez
    yps = derivCoeffsCubic ys
    zs = rzCoeffs3 rbez
    zps = derivCoeffsCubic zs
    x = evalCubic xs t
    xp = evalQuadratic xps t
    y = evalCubic ys t
    yp = evalQuadratic yps t
    z = evalCubic zs t
    zp = evalQuadratic zps t
  in
    makeVector
      ((xp/z)-(x*zp)/z^2)
      ((yp/z)-(y*zp)/z^2)

makeRBezier3 :: WPoint -> WPoint -> WPoint -> WPoint -> RBezier3
makeRBezier3 s@(sp,sz) c@(cp,cz) d@(dp,dz) e@(ep,ez) =
  let
    sx = sp^.x
    sy = sp^.y
    cx = cp^.x
    cy = cp^.y
    dx = dp^.x
    dy = dp^.y
    ex = ep^.x
    ey = ep^.y
  in
    RBezier3
      { rstart3=s
      , rstCont3=c
      , rendCont3=d
      , rend3=e
      , rxCoeffs3 = computeBez3Coeffs (sx*sz,cx*cz,dx*dz,ex*ez) -- (a3,a2,a1,a0)
      , ryCoeffs3 = computeBez3Coeffs (sy*sz,cy*cz,dy*dz,ey*ez) -- (b3,b2,b1,b0)
      , rzCoeffs3 = computeBez3Coeffs (sz,cz,dz,ez)
      , rboundingBox3 = convexBoundingBox [sp,cp,dp,ep]
      }


