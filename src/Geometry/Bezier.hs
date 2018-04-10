--
-- Bezier.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Bezier where

import Geometry.Types
import Geometry.Constructors
import Geometry.Curve.Class
import Geometry.Region.Class
import Geometry.Region
import Geometry.Common
import Geometry.Affine

data Bezier2 = Bezier2 
  { start2 :: Point
  , control2 :: Point
  , end2 :: Point
  , parametrization2 :: Double -> Point
  , implicitization2 :: Point -> Double
  }

makeBezier2 :: Point -> Point -> Point -> Bezier2
makeBezier2 sp@(sx,sy) cp@(cx,cy) ep@(ex,ey) =
  let
    a1 = sx-cy+ex
    a2 = cx-2*ex
    b1 = sy-cy+ey
    b2 = cy-2*ey
    a = sy^2-4*sy*cy + 2*sy*ey + 4*cy^2 - 4*cy*ey + ey^2
    b = sx^2-4*sx*cx + 2*sx*ex + 4*cx^2 - 4*cx*ex + ex^2
    c = -2*sx*sy+4*sx*cy-2*sx*ey+4*cx*sy-8*cx*cy + 4*cx*ey-2*ex*sy + 4 * ex*cy -2*ex*ey
    d = 2*sx*sy*ey - 4*sx*cy^2 + 4*sx*cy*ey-2*sx*ey^2+4*cx*sy*cy - 8*cx*sy*ey + 4*cx*cy*ey - 2*ex*sy^2 
          + 4*ex*sy*cy + 2*ex*sy*ey - 4 *ex*cy^2
    e = 2*sy*sx*ex - 4*sy*cx^2 + 4*sy*cx*ex-2*sy*ex^2+4*cy*sx*cx - 8*cy*sx*ex + 4*cy*cx*ex - 2*ey*sx^2 
          + 4*ey*sx*cx + 2*ey*sx*ex - 4 *ey*cx^2
    f = sx^2*ey^2-4*sx*cx*cy*ey - 2*sx*ex*sy*ey + 4 * sx*ex*cy^2 + 4*cx^2*sy*ey - 4*cx*ex*sy*cy + ex^2*sy^2
  in
    Bezier2
      { start2=sp
      , control2=cp
      , end2=ep
      , parametrization2 = \t -> 
          ( t*(t*a1+a2)+ex
          , t*(t*b1+b2)+ey
          )
      , implicitization2 = \(x,y) -> a*x^2 + b*y^2 + c*x*y + d*x + e*y + f
      }

instance Curve Bezier2 where
  type CurveData Bezier2 = Double
  param = parametrization2
  --polyLine :: CurveData c -> c -> PolyLine
  polyLine = approxPolyLine
  --distance :: CurveData c -> c -> Point -> Double
  distance = approxDistance
  --winding :: CurveData c -> c -> Point -> Double
  winding = approxWinding


data Bezier3 = Bezier3 
  { start3 :: Point
  , stCont3 :: Point
  , endCont3 :: Point
  , end3 :: Point
  , parametrization3 :: Double -> Point
  , implicitization3 :: Point -> Double
  }

--data Bezier = Bezier 



