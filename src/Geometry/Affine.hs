--
-- Affine.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Affine where

import Geometry.Types
import Control.Lens

class Summable a b c | a b -> c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Subtractable a b c | a b -> c where
  infixl 6 -.
  (-.) :: a -> b -> c

class Multiplicable a b c | a b -> c where
  infixr 7 *.
  (*.) :: a -> b -> c

instance Summable Vector Vector Vector where
  --(+.) :: Vector -> Vector -> Vector
  (+.) v1 v2 = v1 & x +~ (v2 ^. x) & y +~ (v2 ^. y)
  -- (x,y) (x',y') = (x+x',y+y')
instance Summable Vector Point Point where
  (+.) v1 v2 = v2 & x +~ (v1 ^. x) & y +~ (v1 ^. y)
instance Summable Point Vector Point where
  (+.) v1 v2 = v1 & x +~ (v2 ^. x) & y +~ (v2 ^. y)
-- TODO: decide whether Point + Point = Point is bad practice or not
instance Summable Point Point Point where
  (+.) p1 p2 = p1 & x +~ (p2 ^. x) & y +~ (p2 ^. y)

instance Subtractable Vector Vector Vector where
  --(-.) :: Vector -> Vector -> Vector
  (-.) v1 v2 = v1 & x -~ (v2 ^. x) & y -~ (v2 ^. y)
  -- (x,y) (x',y') = (x-x',y-y')
instance Subtractable Point Point Vector where
  (-.) v1 v2 = makeVector (v1^.x - v2^.x) (v1^.y - v2^.y)
instance Subtractable Point Vector Point where
  (-.) v1 v2 = v1 & x -~ (v2 ^. x) & y -~ (v2 ^. y)

instance Multiplicable Double Vector Vector where 
  --(*.) :: Double -> Vector -> Vector
  (*.) x v1 = v1 & each *~ x
  --(y,z) = (x*y,x*z)
instance Multiplicable Double Point Point where 
  (*.) x p1 = p1 & each *~ x
instance Multiplicable Double Matrix Matrix where 
  (*.) x mat = mat & each %~ (x *.)
instance Multiplicable Matrix Vector Vector where 
  (*.) mat vec = (vec^.x) *. (mat^.x) +. (vec^.y) *. (mat^.y)
instance Multiplicable Matrix Point Point where 
  (*.) mat pt = (makePoint 0 0) +. (pt^.x) *. (mat^.x) +. (pt^.y) *. (mat^.y)
  -- lol TODO fix this xD
instance Multiplicable Affine Vector Vector where 
  (*.) aff vec = (aff^.linear) *. vec +. (aff^.translation)
instance Multiplicable Affine Point Point where 
  (*.) aff pt = (aff^.linear) *. pt +. (aff^.translation)
instance Multiplicable Vector Point Double where 
  (*.) vc pt = vc^.x*pt^.x+vc^.y*pt^.y
instance Multiplicable Point Vector Double where 
  (*.) pt vc = vc^.x*pt^.x+vc^.y*pt^.y
instance Multiplicable Vector Vector Double where 
  (*.) v1 v2 = v1^.x*v2^.x+v1^.y*v2^.y

--infixl 7 *!
--(*!) :: Matrix -> Vector -> Vector
--(*!) mat vec = (vec ^. x) *. (mat ^. x) +. (vec ^. y) *. (mat ^. y)
--(v1,v2) (x1,x2) = x1 *. v1 +. x2 *.v2

--infixl 7 *!+
--(*!+) :: Affine -> Point -> Point
--(*!+) aff pt = 
-- (m,t) p = m *! p +. t

line :: Point -> Point -> Double -> Point
line p1 p2 t = p1 +. t*.(p2-.p1)

-- vector start end
vector :: Point -> Point -> Vector
vector = (-.)

zeroVector :: Vector
zeroVector = makeVector 0 0

origin :: Point
origin = makePoint 0 0

distancePt :: Point -> Point -> Double
distancePt p1 p2 = vectorNorm $ vector p1 p2

translate :: Vector -> Point -> Point
translate = (+.)

vectorNorm :: Vector -> Double
vectorNorm = sqrt . sqNorm

sqNorm :: Vector -> Double
sqNorm vec =
  let
    xv=vec^.x
    yv=vec^.y
  in
    xv^2+yv^2

-- rotates a vector left 90 degrees
perpendicular :: Vector -> Vector
perpendicular vec = makeVector (-vec^.y) (vec^.x)

normalize :: Vector -> Vector
normalize v = (1/vectorNorm v) *. v

dot :: (Multiplicable a b Double) => a -> b -> Double
dot = (*.)

-- converts a segment to (a,b,c) which are the coeffs of the
-- line containing the segment: ax+by=c, with a^2+b^2=1
segmentToLine :: Segment -> (Vector,Double)
segmentToLine seg =
  let
    n = normalize $ perpendicular (seg^.end -. seg^.start)
  in
    (n,n `dot` (seg^.start))

normalOfSegment :: Segment -> Vector
normalOfSegment = fst . segmentToLine

segmentDistance :: Segment -> Point -> Double
segmentDistance seg pt = 
  let
    p1 = seg^.start
    p2 = seg^.end
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
pointToAngle pt
  | pt^.y > 0 = acos (pt^.x)
  | otherwise = -(acos (pt^.x))

segmentWinding :: Segment -> Point -> Double
segmentWinding seg pt = 
  let
    st = normalize $ seg^.start -. pt
    sx=st^.x
    sy=st^.y
    et = normalize $ seg^.end -. pt
    ex=et^.x
    ey=et^.y
    crss = sx*ey-sy*ex
    d = st `dot` et
    theta = pointToAngle $ makePoint d crss
  in
    theta/(2* pi)

-- TODO, wrong place for this constructor. Refactor
-- the halfplane
-- v . x >= d
-- contructor normalizes v
buildHalfPlane :: Vector -> Double -> HalfPlane
buildHalfPlane v d =
  let
    s = vectorNorm v
  in 
    HalfPlane (s *. v) (s*d)
-- TODO, again a constructor. Refactor.
-- guarantees the point is in the halfplane
buildHalfPlanePoint :: Point -> Vector -> Double -> HalfPlane
buildHalfPlanePoint p v d = 
  if p `dot` v >= d
  then
    makeHalfPlane v d
  else 
    makeHalfPlane (zeroVector -. v) (-d)

insideHalfPlane :: HalfPlane -> Point -> Bool
insideHalfPlane hp pt = (hp^.normal) `dot` pt >= hp^.radius

insideConvtope :: ConvexPolytope -> Point -> Bool
insideConvtope conv pt = allOf hplanes (flip insideHalfPlane pt) conv

avgPoints :: [Point] -> Point
avgPoints ps = (1/fromIntegral (length ps) :: Double) *. foldr (+.) origin ps

-- makePolygon vertices isConvex
buildPolygon :: [Point] -> Bool -> Polygon
buildPolygon ps isConvex = 
  let
    pl = makePolyLine (ps ++ [head ps])
    center = avgPoints ps
  in 
    makePolygon pl $
      if isConvex
      then
        Just 
          . makeConvexPolytope
          $ pl
              ^.. segments
              & each 
              %~ (uncurry (buildHalfPlanePoint center) . segmentToLine)
      else
        Nothing

makeBoxSides :: Double -> Double -> Double -> Double -> Box
makeBoxSides lx mx ly my =
  let
    llc = (lx,ly)^.from ptAsPair
    urc = (mx,my)^.from ptAsPair
  in
    makeBoxCorners llc urc

makeBoxCorners :: Point -> Point -> Box
makeBoxCorners llc urc = makeBox llc (urc-.llc)
