--
-- Geometry.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

-- 2D geometry module for use with animation
module Geometry where
  ( module Geometry
  , module Geometry.Curve
  , module Geometry.Types
  ) where

import Data.Array 

import Geometry.Curve
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

makePoint :: Double -> Double -> Point
makePoint = (,)

makeVector :: Double -> Double -> Vector
makeVector = (,)

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


makePolyLine :: [Point] -> PolyLine
makePolyLine ps = PolyLine $ listArray (0,fromIntegral $ length ps - 1) ps


avgPoints :: [Point] -> Point
avgPoints ps = (1/fromIntegral (length ps)) *. foldr (+.) (0,0) ps

-- makePolygon vertices isConvex
makePolygon :: [Point] -> Bool -> Polygon
makePolygon ps isConvex = 
  let
    pl = makePolyLine (ps ++ [head ps])
    center = avgPoints ps
  in 
    Polygon 
      { polyBdry = pl
      , polyRegion =
          if isConvex
          then
            Just 
              . makeConvexPolytope
              . segmentFold
                  (uncurry (makeHalfPlanePoint center) . segmentToLine)
                  (:)
                  []
              $ pl
          else
            Nothing
      }


makeSegment :: Point -> Point -> Segment
makeSegment = (,)


approxPolyLine :: (Curve c) => Double -> c -> PolyLine
approxPolyLine interval curve =
  let
    n = ceiling (1/interval) :: Integer
    delta = 1/ (fromIntegral n) :: Double
    f = \x -> param curve $ (fromIntegral x) * delta
  in
    makePolyLine $ map f [0..n]

{-
chooseFunction :: (Double -> f) -> (Maybe f) -> Double -> f
chooseFunction f Nothing = f 
chooseFunction _ (Just x) = const x

-}
--polyLine :: (Curve c) => Double -> c -> PolyLine
--polyLine = chooseFunction approxPolyLine exactPolyLine


approxDistance :: (Curve c) => CurveData c -> c -> Point -> Double
approxDistance int curve = polyLineDistance (polyLine int curve)

--distance :: (Curve c) => Double -> c -> Point -> Double
--distance = chooseFunction approxDistance exactDistance

approxWinding :: (Curve c) => CurveData c -> c -> Point -> Double
approxWinding int curve = polyLineWinding (polyLine int curve)

--winding :: (Curve c) => Double -> c -> Point -> Double
--winding = chooseFunction approxWinding exactWinding

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


instance Curve Segment where
  type CurveData Segment = ()
  param (p1,p2) = line p1 p2
  polyLine () (p1,p2) = makePolyLine [p1,p2]
  distance () = segmentDistance
  winding () = segmentWinding
  --exactDistance (p1,p2) = min 

segmentFold :: (Segment -> a) -> (a -> b -> b) -> b -> PolyLine -> b
segmentFold f op init (PolyLine arr) = 
  let
    ixs = range $ bounds arr
    segixs = zip ixs $ tail ixs
    segvals = map (\(ix1,ix2) -> f (arr ! ix1, arr! ix2)) segixs
  in
    foldr op init segvals

segmentFold1 :: (Segment -> a) -> (a -> a -> a) -> PolyLine -> a
segmentFold1 f op (PolyLine arr) = 
  let
    ixs = range $ bounds arr
    segixs = zip ixs $ tail ixs
    segvals = map (\(ix1,ix2) -> f (arr ! ix1, arr! ix2)) segixs
  in
    foldr1 op segvals

polyLineDistance :: PolyLine -> Point -> Double
polyLineDistance pl pt = segmentFold1 (flip segmentDistance pt) min pl

polyLineWinding :: PolyLine -> Point -> Double
polyLineWinding pl pt = segmentFold (flip segmentWinding pt) (+) 0 pl

instance Curve PolyLine where
  type CurveData PolyLine = ()
  param (PolyLine arr) t = 
    let
      (l,r) = bounds arr
      ts = t*(fromIntegral (r-l)) -- rescaled to match 0-1 per segment of the polyline
      ix = floor $ ts -- index of beginning of segment
      t' = ts - fromIntegral ix -- index in segment
    in
      param (arr ! ix,arr!(ix+1)) t'
  polyLine () = id
  distance () = polyLineDistance
  winding () = polyLineWinding
  --exactDistance = 

instance Curve Polygon where
  type CurveData Polygon = ()
  param Polygon{..} t = param polyBdry t
  polyLine () Polygon{..} = polyBdry
  distance () Polygon{..} pt = polyLineDistance polyBdry pt
  winding () Polygon{..} pt = polyLineWinding polyBdry pt

polygonWindingNumber :: Polygon -> Point -> Integer
polygonWindingNumber Polygon{..} pt = case polyRegion of
  Just convex ->
    if inside () convex pt
    then
      1
    else
      0
  Nothing ->
    approxWindingNumber () polyBdry pt

polygonInside :: Polygon -> Point -> Bool
polygonInside Polygon{..} pt = case polyRegion of
  Just convex ->
    inside () convex pt
  Nothing ->
    (approxWindingNumber () polyBdry pt) /= 0

instance ClosedCurve Polygon where
  insideCC () = polygonInside
  windingNumber () = polygonWindingNumber

makeCircle :: Point -> Double -> Circle
makeCircle = Circle

instance Curve Circle where
  --type CurveData Circle = ()
  param circle t = 
    let
      theta = t*2*pi
    in
      (radius circle * sin theta, radius circle * cos theta)
  polyLine = approxPolyLine
  --exactDistance = Just $ \circ pt -> abs $ radius circ - distancePt pt (center circ)
  --exactWinding = Just $ \circ pt -> 
    --if distancePt pt (center circ) <= radius circ 
    --then
      --1
    --else 
      --0


instance ClosedCurve Circle where
  windingNumber _ = \circ pt ->
    if radius circ >= distancePt pt (center circ)
    then
      1
    else
      0


class Curve c => ClosedCurve c where
  insideCC :: CurveData c -> c -> Point -> Bool -- don't need to define if you define exactWindingNumber, unless
  -- you have a more efficient variant
  insideCC = approxInside
  windingNumber :: CurveData c -> c -> Point -> Integer
  windingNumber = approxWindingNumber



class Region r where
  type RegionData r :: *
  type RegionData r = Double
  inside :: RegionData r -> r -> Point -> Bool
  distanceBdry :: Maybe (RegionData r -> r -> Point -> Double)
  

approxWindingNumber :: Curve c => CurveData c -> c -> Point -> Integer
approxWindingNumber val curve pt = round $ winding val curve pt

--windingNumber :: ClosedCurve c => CurveData c -> c -> Point -> Integer
--windingNumber = chooseFunction approxWindingNumber exactWindingNumber

approxInside :: (ClosedCurve c) => CurveData c -> c -> Point -> Bool
approxInside val curve pt = (windingNumber val curve pt) /= 0

--insideCC :: (ClosedCurve c) => CurveData c -> c -> Point -> Bool
--insideCC = chooseFunction approxInside exactInside

--newtype ClosedCurveRegion :: * where
  --ClosedCurveRegion :: (ClosedCurve c) => c -> ClosedCurveRegion

instance Region Polygon where
  type RegionData Polygon = ()
  inside = insideCC
  distanceBdry = Just distance

instance Region Circle where
  type RegionData Circle = ()
  inside () = insideCC 0.0
  distanceBdry = Just $ \() -> distance 0.0 

{-
instance (ClosedCurve c) => Region c where
instance Region ClosedCurve where
  inside = insideCC
  distanceBdry = Just distance
-}

-- distance from a point to a region
distanceTo :: (Region r) => RegionData r -> r -> Point -> Maybe Double
distanceTo val reg pt = 
  if not $ inside val reg pt
  then
    distanceBdry >>= (\f -> return $ f val reg pt)
  else
    Just 0

-- distance from a point in a region to the exterior of the region
distanceOut :: (Region r) => RegionData r -> r -> Point -> Maybe Double
distanceOut val reg pt = 
  if not $ inside val reg pt
  then
    distanceBdry >>= (\f -> return $ f val reg pt)
  else
    Just 0

-- the halfplane
-- v . x >= d
-- contructor normalizes v
makeHalfPlane :: Vector -> Double -> HalfPlane
makeHalfPlane v d =
  let
    s = vectorNorm v
  in 
    HalfPlane (s *. v) (s*d)

-- guarantees the point is in the halfplane
makeHalfPlanePoint :: Point -> Vector -> Double -> HalfPlane
makeHalfPlanePoint p v d = 
  if p `dot` v >= d
  then
    makeHalfPlane v d
  else 
    makeHalfPlane ((0,0)-.v) (-d)

instance Region HalfPlane where
  type RegionData HalfPlane = ()
  inside _ (HalfPlane n d) pt = n `dot` pt >= d
  distanceBdry = Just $ \_ (HalfPlane n d) pt -> abs ((n `dot` pt)-d)


makeConvexPolytope :: [HalfPlane] -> ConvexPolytope
makeConvexPolytope = ConvexPolytope

instance Region ConvexPolytope where
  type RegionData ConvexPolytope = ()
  inside _ (ConvexPolytope hps) pt = and $ map (flip (inside ()) pt) hps
  distanceBdry = Nothing


instance Region ImplicitRegion where
  type RegionData ImplicitRegion = ()
  inside _ (ImplicitRegion f) pt = f pt
  distanceBdry = Nothing


{-
  approxPolygon :: Double -> c -> Polygon
  approxPolygon interval curve =
    let
      n = ceiling (1/interval) :: Integer
      delta = 1/ (fromIntegral n) :: Double
      f = \x -> param curve $ (fromIntegral x) * delta
    in
      makePolygon $ map f [0..(n-1)]
  distance :: Double -> c -> Point -> Double

class ClosedCurve c => ClosedBoundaryCurve c where
  windingNumber :: c -> Point -> Integer


class Curve c => BoundaryCurve c where
  -- partial winding of the curve around the point
  winding :: c -> Point -> Double

-}

