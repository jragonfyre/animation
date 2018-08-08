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

import Control.Lens ((^.), from, (&),(.~))

import Utils

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

instance GBounded Bezier2 where
  bounds = boundingBox2

instance Geometric Bezier2 where
  transform aff Bezier2{start2=s,control2=c,end2=e} =
    makeBezier2
      (transform aff s)
      (transform aff c)
      (transform aff e)

parametrization2 :: Bezier2 -> Double -> Point
parametrization2 bez t =
  let 
    (a2,a1,a0) = xCoeffs2 bez
    (b2,b1,b0) = yCoeffs2 bez
  in
    makePoint
      (t*(t*a2+a1)+a0)
      (t*(t*b2+b1)+b0)

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
    a1 = sx-2*cx+ex
    a2 = -2*sx+2*cx
    b1 = sy-2*cy+ey
    b2 = -2*sy+2*cy
    a = sy^2-4*sy*cy + 2*sy*ey + 4*cy^2 - 4*cy*ey + ey^2
    b = sx^2-4*sx*cx + 2*sx*ex + 4*cx^2 - 4*cx*ex + ex^2
    c = -2*sx*sy+4*sx*cy-2*sx*ey+4*cx*sy-8*cx*cy + 4*cx*ey-2*ex*sy + 4 * ex*cy -2*ex*ey
    d = 2*sx*sy*ey - 4*sx*cy^2 + 4*sx*cy*ey-2*sx*ey^2+4*cx*sy*cy - 8*cx*sy*ey + 4*cx*cy*ey - 2*ex*sy^2 
          + 4*ex*sy*cy + 2*ex*sy*ey - 4 *ex*cy^2
    e = 2*sy*sx*ex - 4*sy*cx^2 + 4*sy*cx*ex-2*sy*ex^2+4*cy*sx*cx - 8*cy*sx*ex + 4*cy*cx*ex - 2*ey*sx^2 
          + 4*ey*sx*cx + 2*ey*sx*ex - 4 *ey*cx^2
    f = sx^2*ey^2-4*sx*cx*cy*ey - 2*sx*ex*sy*ey + 4 * sx*ex*cy^2 + 4*cx^2*sy*ey - 4*cx*ex*sy*cy + ex^2*sy^2
    (lx,mx) = minMaxOf [sx,cx,ex]
    (ly,my) = minMaxOf [sy,cy,ey]
  in
    Bezier2
      { start2=sp
      , control2=cp
      , end2=ep
      , xCoeffs2 = (a1,a2,sx)
      , yCoeffs2 = (b1,b2,sy)
      , implicitParams2 = (a,b,c,d,e,f)
      , boundingBox2 = makeBoxSides lx mx ly my
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
    a3 = -sx+3*cx-3*dx+ex
    a2 = 3*sx-6*cx+3*dx
    a1 = -3*sx+3*cx
    a0 = sx
    b3 = -sy+3*cy-3*dy+ey
    b2 = 3*sy-6*cy+3*dy
    b1 = -3*sy+3*cy
    b0 = sy
    boundBox = convexBoundingBox [sp,cp1,cp2,ep]
  in
    Bezier3
      { start3=sp
      , stCont3=cp1
      , endCont3=cp2
      , end3=ep
      , xCoeffs3 = (a3,a2,a1,a0)
      , yCoeffs3 = (b3,b2,b1,b0)
      , boundingBox3 = boundBox
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





