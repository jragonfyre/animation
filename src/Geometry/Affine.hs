--
-- Affine.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Affine 
  ( module Geometry.Affine
  , module MathClasses
  ) where

import Geometry.Types
import PolynomialSolver
import MathClasses

import Control.Lens (over, (&), (^.), (%~), from, each, (^..), (*~), (+~), (-~), allOf)
import Control.Lens.Traversal

import Data.Maybe (mapMaybe)

import Utils

class Geometric a where
  transform :: Affine -> a -> a

-- objects determined by points
class Pointed a where
  -- forall f. Applicative f => (Point -> f Point) -> a -> f a
  pointsOf :: Traversal' a Point

instance Traversable t => Pointed (t Point) where
  pointsOf = traverse

{-
instance Pointed a => Geometric a where
  transform aff pted = pted & pointsOf %~ (aff *.)
-}

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
instance Subtractable Matrix Matrix Matrix where
  (-.) mat1 mat2 = mat1 & x %~ (-.(mat2^.x)) & y %~ (-.(mat2^.y))

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
  (*.) aff vec = (aff^.linear) *. vec
instance Multiplicable Affine Point Point where 
  (*.) aff pt = (aff^.linear) *. pt +. (aff^.translation)
instance Multiplicable Vector Point Double where 
  (*.) vc pt = vc^.x*pt^.x+vc^.y*pt^.y
instance Multiplicable Point Vector Double where 
  (*.) pt vc = vc^.x*pt^.x+vc^.y*pt^.y
instance Multiplicable Vector Vector Double where 
  (*.) v1 v2 = v1^.x*v2^.x+v1^.y*v2^.y

instance Polynomializable Vector Vector where
instance Polynomializable Vector Point where

instance Geometric Vector where
  transform = (*.)

instance Pointed Point where
  pointsOf inj pt = inj pt
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

instance Geometric ConvexPolytope where
  transform aff cp = cp & hplanes %~ (transform aff)

line :: Point -> Point -> Double -> Point
line p1 p2 t = p1 +. t*.(p2-.p1)

segmentParametrization :: Segment -> Double -> Point
segmentParametrization seg = line (seg^.start) (seg^.end)

-- vector start end
vector :: Point -> Point -> Vector
vector = (-.)

unitDirection :: Double -> Vector
unitDirection theta = makeVector (cos theta) (sin theta)

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

-- rotates a vector right 90 degrees, i.e. by -90 degrees
-- this is chosen so that using the usual notion of orientation from mathematics, we get
-- that perpendicular . tangentVec = normalVec to a curve, with the normal pointing outwards
-- (rather than inwards)
perpendicular :: Vector -> Vector
perpendicular vec = makeVector (vec^.y) (-vec^.x)

-- rotates a vector left 90 degrees, i.e. by +90 degrees
perpendicularLeft :: Vector -> Vector
perpendicularLeft vec = makeVector (-vec^.y) (vec^.x)

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

cross :: Vector -> Vector -> Double
cross v1 v2 = det (makeMatrix v1 v2)

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

sign :: Double -> Double
sign x | x >= 0 = 1
       | otherwise = -1

angleFrom :: Vector -> Vector -> Double
angleFrom u v = (sign (cross u v)) * (acos $ max (-1) $ min 1 ((u`dot`v)/((vectorNorm u) * (vectorNorm v))))

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

makeBoxSides :: Double -> Double -> Double -> Double -> Box
makeBoxSides lx mx ly my =
  let
    llc = (lx,ly)^.from ptAsPair
    urc = (mx,my)^.from ptAsPair
  in
    makeBoxCorners llc urc

makeBoxCorners :: Point -> Point -> Box
makeBoxCorners llc urc = makeBox llc (urc-.llc)

-- width, height are the radii
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

segmentBoundingBox :: Segment -> Box
segmentBoundingBox = convexBoundingBox . (^.. each)

unionBoxes :: [Box] -> Box
unionBoxes boxes = 
  let 
    lx = minimum $ map boxLeft boxes
    mx = maximum $ map boxRight boxes
    ly = minimum $ map boxBottom boxes
    my = maximum $ map boxTop boxes
  in 
    makeBoxSides
      lx
      mx
      ly
      my

intersectionBoxes :: [Box] -> Box
intersectionBoxes boxes = 
  let 
    lx = maximum $ map boxLeft boxes
    mx = minimum $ map boxRight boxes
    ly = maximum $ map boxBottom boxes
    my = minimum $ map boxTop boxes
  in 
    makeBoxSides
      lx
      mx
      ly
      my

instance (GBounded a) => GBounded [a] where
  bounds = unionBoxes . map bounds

instance GBounded Point where
  bounds = flip makeBox zero

instance GBounded Segment where
  bounds = segmentBoundingBox

instance GBounded Box where
  bounds = id

instance Geometric Segment where
  transform aff seg = seg & each %~ (transform aff)

withinY :: Box -> Double -> Bool
withinY bx y = (boxBottom bx) <= y && y <= (boxTop bx)

withinX :: Box -> Double -> Bool
withinX bx x = (boxLeft bx) <= x && x <= (boxRight bx)

rotate :: Double -> Matrix
rotate theta =
  let
    ct = cos theta
    st = sin theta
  in 
    ((ct,st),(-st,ct))^.from matAsComponents

diagonal :: Double -> Double -> Matrix
diagonal a d = ((a,0),(0,d))^.from matAsComponents

scale :: Double -> Matrix
scale s = diagonal s s

matrixToAffine :: Matrix -> Affine
matrixToAffine mat = makeAffine mat zero

translate :: Vector -> Affine
translate = makeAffine unit

translateToOrigin :: Point -> Affine
translateToOrigin = translate . (origin -.)

trace :: Matrix -> Double
trace mat = 
  let
    ((a,_),(_,d)) = mat^.matAsComponents
  in
    a + d

matrixEigenvalues :: Matrix -> [Double]
matrixEigenvalues mat = 
  case solveQuadratic (1,-(trace mat), det mat) of 
    [] ->
      []
    [ev] -> [ev]
    [ev1,ev2] -> if ev1==ev2 then [ev1] else [ev1,ev2]
    _ -> []

matrixEigenvaluesTolerance :: Double -> Matrix -> [Double]
matrixEigenvaluesTolerance tol mat = 
  case solveQuadratic (1,-(trace mat), det mat) of 
    [] ->
      []
    [ev] -> [ev]
    [ev1,ev2] -> if abs (ev1-ev2) < tol then [ev1] else [ev1,ev2]
    _ -> []

data Subspace = ZeroSubSp | OneDSubSp Vector | WholeSubSp
  deriving (Show, Eq, Ord, Read)

-- suffers from rounding error (numerical instability)
matrixKernel :: Matrix -> Subspace
matrixKernel mat =
  if det mat /= 0
  then 
    ZeroSubSp
  else
    if mat == zero
    then
      WholeSubSp
    else
      let
        ((a,c),(b,d)) = mat^.matAsComponents
      in 
        OneDSubSp $ 
          if a == 0
          then
            if b == 0
            then
              normalize $ makeVector (-d) c
            else -- c==0, since det mat == 0
              makeVector 1 0
          else
            normalize $ makeVector (-b) a

matrixKernelTolerance :: Double -> Matrix -> Subspace
matrixKernelTolerance tol mat =
  if abs (det mat) >= tol
  then 
    ZeroSubSp
  else
    let
      ((a,c),(b,d)) = mat^.matAsComponents
    in 
      if (abs a < tol) && (abs b < tol) && (abs c < tol) && (abs d < tol)
      then
        WholeSubSp
      else
        OneDSubSp $ 
          if abs a < tol
          then
            if abs b < tol
            then
              normalize $ makeVector (-d) c
            else -- c==0, since det mat == 0
              makeVector 1 0
          else
            normalize $ makeVector (-b) a

-- eigenvalue, eigenvector pairs
matrixEigenvectors :: Matrix -> [(Double,Vector)]
matrixEigenvectors mat = 
  let
    f ev = case matrixKernel (mat -. (scale ev)) of 
      ZeroSubSp -> 
        []
      WholeSubSp ->
        [(ev,makeVector 1 0),(ev,makeVector 0 1)]
      OneDSubSp vec ->
        [(ev,vec)]
  in
    concat $ map f $ matrixEigenvalues mat

matrixEigenvectorsTolerance :: Double -> Matrix -> [(Double,Vector)]
matrixEigenvectorsTolerance tol mat = 
  let
    f ev = case matrixKernelTolerance tol (mat -. (scale ev)) of 
      ZeroSubSp -> 
        []
      WholeSubSp ->
        [(ev,makeVector 1 0),(ev,makeVector 0 1)]
      OneDSubSp vec ->
        [(ev,vec)]
  in
    concat $ map f $ matrixEigenvaluesTolerance tol mat


symmetrizeMatrix :: Matrix -> Matrix
symmetrizeMatrix mat =
  let
    ((a,c),(b,d)) = mat^.matAsComponents
  in
    ((a,(b+c)/2),((b+c)/2,d))^.from matAsComponents

boxToCenterAndRadii :: Box -> (Point,Double,Double)
boxToCenterAndRadii box = 
  let
    corn = box^.corner
    dim = box^.dimensions
    d2 = (0.5::Double) *. dim
  in
    (corn+.d2,d2^.x,d2^.y)

