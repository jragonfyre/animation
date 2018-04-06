--
-- Affine.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Affine where

import Geometry.Types

infixl 6 +.
(+.) :: Vector -> Vector -> Vector
(+.) (x,y) (x',y') = (x+x',y+y')

infixl 6 -.
(-.) :: Vector -> Vector -> Vector
(-.) (x,y) (x',y') = (x-x',y-y')

infixl 7 *.
(*.) :: Double -> Vector -> Vector
(*.) x (y,z) = (x*y,x*z)

infixl 7 *!
(*!) :: M2 -> Vector -> Vector
(*!) (v1,v2) (x1,x2) = x1 *. v1 +. x2 *.v2

infixl 7 *!+
(*!+) :: A2 -> Point -> Point
(*!+) (m,t) p = m *! p +. t

line :: Point -> Point -> Double -> Point
line p1 p2 t = t *. p1 +. (1-t) *. p2

-- vector start end
vector :: Point -> Point -> Vector
vector (x,y) (x',y') = (x'-x,y'-y)

distancePt :: Point -> Point -> Double
distancePt p1 p2 = vectorNorm $ vector p1 p2

translate :: Vector -> Point -> Point
translate (x,y) (px,py) = (px+x,py+y)

vectorNorm :: Vector -> Double
vectorNorm (x,y) = sqrt $ x*x+y*y

sqNorm :: Vector -> Double
sqNorm (x,y) = x*x + y*y

perpendicular :: Vector -> Vector
perpendicular (x,y) = (-y,x)

normalize :: Vector -> Vector
normalize v = (1/vectorNorm v) *. v

dot :: Vector -> Vector -> Double
dot (x1,y1) (x2,y2) = x1*x2+y1*y2

-- converts a segment to (a,b,c) which are the coeffs of the
-- line containing the segment: ax+by=c, with a^2+b^2=1
segmentToLine :: Segment -> (Vector,Double)
segmentToLine (p1,p2) =
  let
    n = normalize $ perpendicular (p2 -. p1)
  in
    (n,n `dot` p1)

normalOfSegment :: Segment -> Vector
normalOfSegment = fst . segmentToLine

segmentDistance :: Segment -> Point -> Double
segmentDistance (p1,p2) pt = 
  let
    p2' = p2 -. p1
    pt' = pt -. p1
    --p2dir = normalize p2'
    n = normalize $ perpendicular p2'
    --projPt = pt' -. (n `dot` pt') *. n
    c = (p2' `dot` pt')/(sqNorm p2')
  in
    if c < 0
    then
      distancePt p1 pt
    else
      if c > 1
      then
        distancePt p2 pt
      else
        abs (n `dot` pt')

pointToAngle :: Point -> Double
pointToAngle (x,y)
  | y > 0 = acos x
  | otherwise = -(acos x)

segmentWinding :: Segment -> Point -> Double
segmentWinding (start,end) pt = 
  let
    st@(sx,sy) = normalize $ start -. pt
    et@(ex,ey) = normalize $ end -. pt
    crss = sx*ey-sy*ex
    d = st `dot` et
    theta = pointToAngle (d,crss)
  in
    theta/(2* pi)

