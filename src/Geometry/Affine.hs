--
-- Affine.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Affine where

import Geometry.Types
import Control.Lens (over, (&), (^.), (%~), from, each, (^..), (*~), (+~), (-~), allOf)

import Utils

class Summable a b c | a b -> c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Subtractable a b c | a b -> c where
  infixl 6 -.
  (-.) :: a -> b -> c

class Multiplicable a b c | a b -> c where
  infixr 7 *.
  (*.) :: a -> b -> c

class Negatable a where
  negify :: a -> a

class Zeroable a where
  zero :: a

class Geometric a where
  transform :: Affine -> a -> a

class Unitable a where
  unit :: a

class Transposable a b | a -> b where
  transpose :: a -> b

instance Transposable Vector Covector where
  transpose = Covector

instance Transposable Covector Vector where
  transpose = _covectorDual

instance Transposable Matrix Matrix where
  transpose mat = 
    let ((a,c), (b,d)) = mat^.matAsComponents
    in
      ((a,b),(c,d))^.from matAsComponents

instance Unitable Matrix where
  unit = ((1,0),(0,1))^.from matAsComponents

instance Unitable Affine where
  unit = makeAffine unit zero

instance Zeroable Vector where
  zero = makeVector 0 0 
deriving instance Zeroable Covector

instance Zeroable Matrix where
  zero = makeMatrix zero zero

instance Negatable Vector where
  negify vec = vec & each %~ negate
deriving instance Negatable Covector

instance Negatable Matrix where
  negify mat = mat & each %~ negify

instance Summable Vector Vector Vector where
  --(+.) :: Vector -> Vector -> Vector
  (+.) v1 v2 = v1 & x +~ (v2 ^. x) & y +~ (v2 ^. y)
  -- (x,y) (x',y') = (x+x',y+y')
instance Summable Covector Covector Covector where
  (+.) cv1 cv2 = ((cv1^.from dualize)+.(cv2^.from dualize))^.dualize
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
instance Subtractable Covector Covector Covector where
  (-.) cv1 cv2 = ((cv1^.from dualize)-.(cv2^.from dualize))^.dualize
instance Subtractable Point Point Vector where
  (-.) v1 v2 = makeVector (v1^.x - v2^.x) (v1^.y - v2^.y)
instance Subtractable Point Vector Point where
  (-.) v1 v2 = v1 & x -~ (v2 ^. x) & y -~ (v2 ^. y)

instance Multiplicable Double Vector Vector where 
  --(*.) :: Double -> Vector -> Vector
  (*.) x v1 = v1 & each *~ x
  --(y,z) = (x*y,x*z)
instance Multiplicable Double Covector Covector where
  (*.) x cv1 = cv1 & each *~ x
instance Multiplicable Covector Vector Double where
  (*.) cv v = (cv^.from dualize) `dot` v
instance Multiplicable Double Point Point where 
  (*.) x p1 = p1 & each *~ x
instance Multiplicable Double Matrix Matrix where 
  (*.) x mat = mat & each %~ (x *.)
instance Multiplicable Matrix Vector Vector where 
  (*.) mat vec = (vec^.x) *. (mat^.x) +. (vec^.y) *. (mat^.y)
instance Multiplicable Covector Matrix Covector where
  (*.) covec mat = covec & over (from dualize) ((transpose mat)*.)
instance Multiplicable Matrix Point Point where 
  (*.) mat pt = (makePoint 0 0) +. (pt^.x) *. (mat^.x) +. (pt^.y) *. (mat^.y)
  -- lol TODO fix this xD
instance Multiplicable Matrix Matrix Matrix where
  (*.) mat1 mat2 = mat2 & each %~ (mat1 *.)
instance Multiplicable Affine Matrix Affine where
  (*.) aff mat =
    let
      (m1,trans) = aff^.affAsPair
    in
      makeAffine (m1*.mat) trans
instance Multiplicable Matrix Affine Affine where
  (*.) mat aff = 
    let
      (m1,trans) = aff^.affAsPair
    in
      makeAffine (mat*.m1) (mat*.trans)
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

instance Geometric Vector where
  transform = (*.)

instance Geometric Point where
  transform = (*.)

-- should satisfy something like
--
-- (transform pt) `inside` (transform hp) == pt `inside` hp
-- i.e. tnorm `dot` ((mat * pt) + trans) >= trad <=> 
-- norm `dot` pt >= rad
-- if we let tnorm = (transpose (inverse mat)) * norm, we have
-- tnorm `dot` ((mat *pt) + trans) = norm `dot` pt + norm `dot` (inverse mat) * trans 
instance Geometric HalfPlane where
  transform aff hp = 
    let
      (mat, trans) = aff^.affAsPair
      (norm, r) = hp^.hpAsPair
      imat = invertMatrix mat
      tmat = transpose imat
      tnorm = tmat *. norm
      trad = r + (tnorm `dot` trans)
    in
      makeHalfPlane tnorm trad

instance Geometric PolyLine where
  transform aff pl = pl & points %~ (transform aff)

instance Geometric ConvexPolytope where
  transform aff cp = cp & hplanes %~ (transform aff)

instance Geometric Polygon where
  transform aff poly = 
    let 
      pl = poly^.boundary
      cp = poly^.region
    in
      makePolygon (transform aff pl) (fmap (transform aff) cp)

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

origin :: Point
origin = makePoint 0 0

distancePt :: Point -> Point -> Double
distancePt p1 p2 = vectorNorm $ vector p1 p2

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

det :: Matrix -> Double
det mat = 
  let 
    (v1,v2) = mat^.matAsPair
    (a,c) = v1^.vecAsPair
    (b,d) = v2^.vecAsPair
  in
    a*d-b*c

invertMatrix :: Matrix -> Matrix
invertMatrix mat = 
  let 
    ((a,c),(b,d)) = mat^.matAsComponents
    det = a*d-b*c
  in
    (1/det) *. (((d,-c),(-b,a))^.from matAsComponents)

invertAffine :: Affine -> Affine
invertAffine aff = 
  let
    (mat, trans) = aff^.affAsPair
    imat = invertMatrix mat
    ntrans = imat *. (negify trans)
  in
    makeAffine imat ntrans
    

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
    makeHalfPlane (negify v) (-d)

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

makeBoxCenter :: Point -> Double -> Double -> Box
makeBoxCenter cent width height = 
  let
    v = makeVector width height
    two = 2 :: Double
  in 
    makeBox (cent -. v) (two *. v)

makeBoxCenterARHeight :: Point -> Double -> Double -> Box
makeBoxCenterARHeight c ar h = makeBoxCenter c (ar*h) h

convexBoundingBox :: [Point] -> Box
convexBoundingBox pts = 
  let
    (lx,mx) = minMaxOf $ map (^.x) pts
    (ly,my) = minMaxOf $ map (^.y) pts
  in
    makeBoxSides lx mx ly my

polyLineBoundingBox :: PolyLine -> Box
polyLineBoundingBox = convexBoundingBox . (^..points)

polygonBoundingBox :: Polygon -> Box
polygonBoundingBox = convexBoundingBox . (^..boundary.points)

segmentBoundingBox :: Segment -> Box
segmentBoundingBox = convexBoundingBox . (^.. each)

withinY :: Box -> Double -> Bool
withinY bx y = (boxBottom bx) <= y && y <= (boxTop bx)

withinX :: Box -> Double -> Bool
withinY bx x = (boxLeft bx) <= x && x <= (boxRight bx)

rotate :: Double -> Matrix
rotate theta =
  let
    ct = cos theta
    st = sin theta
  in 
    ((ct,st),(-st,ct))^.from matAsComponents

scale :: Double -> Matrix
scale s = ((s,0),(0,s))^.from matAsComponents

matrixToAffine :: Matrix -> Affine
matrixToAffine mat = makeAffine mat zero

translate :: Vector -> Affine
translate = makeAffine unit

