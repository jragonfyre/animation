--
-- Path.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Path
  ( module Geometry.Path
  ) where

import qualified Geometry.Types as T
import Geometry.Types hiding (Vector)
import Geometry.Bezier
import Geometry.Ellipse
import Geometry.Affine

import Polynomial
import PolynomialSolver

import qualified Data.Vector as V
import Data.Vector ((!),imap,Vector)

import Data.Maybe (fromMaybe)

import Control.Lens ((^.))

import qualified Data.List as L


data PathSegment
  = PathSeg !Point
  | PathBez2 !Point !Point
  | PathBez3 !Point !Point !Point
  | PathRBez2 !WPoint !WPoint
  | PathRBez3 !WPoint !WPoint !WPoint
  | PathEArc !Point !Matrix !Bool !Bool !Double
  deriving (Show, Eq, Ord, Read)

makeArcSegment :: Point -> (Double,Double,Double) -> Bool -> Bool -> Double -> PathSegment
makeArcSegment s = PathEArc s . ellipseRadiiToMatrix

makeCircleSegment :: Point -> Double -> Bool -> Double -> PathSegment
makeCircleSegment s rad fS tol = makeArcSegment s (rad,rad,0) False fS tol

makeBez2Seg :: Bezier2 -> PathSegment
makeBez2Seg bez = PathBez2 (start2 bez) (control2 bez)

makeBez3Seg :: Bezier3 -> PathSegment
makeBez3Seg bez = PathBez3 (start3 bez) (stCont3 bez) (endCont3 bez)

makeRBez2Seg :: WPoint -> WPoint -> Double -> PathSegment
makeRBez2Seg (s,sw) (c,cw) ew = PathRBez2 (s,sw/ew) (c,cw/ew)

makeRBez3Seg :: WPoint -> WPoint -> WPoint -> Double -> PathSegment
makeRBez3Seg (s,sw) (c,cw) (d,dw) ew = PathRBez3 (s,sw/ew) (c,cw/ew) (d,dw/ew)

instance Geometric PathSegment where
  transform aff (PathSeg pt) = PathSeg (transform aff pt)
  transform aff (PathBez2 s c) = PathBez2 (transform aff s) (transform aff c)
  transform aff (PathBez3 s c d) = PathBez3 (transform aff s) (transform aff c) (transform aff d)
  transform aff (PathRBez2 s c) = PathRBez2 (transformWPoint aff s) (transformWPoint aff c)
  transform aff (PathRBez3 s c d) = PathRBez3 (transformWPoint aff s) (transformWPoint aff c) (transformWPoint aff d)
  transform aff (PathEArc s mat fA fS tol) =
    let
      (m,_) = aff^.affAsPair
      ns = aff*.s
      mi = invertMatrix m
      nmat = (transpose mi) *. mat *. mi
      d = det m
      nfS = if d < 0 then not fS else fS
    in
      PathEArc ns nmat fA nfS tol

pSegStart :: PathSegment -> Point
pSegStart (PathSeg s) = s
pSegStart (PathBez2 s _) = s
pSegStart (PathBez3 s _ _) = s
pSegStart (PathRBez2 (s,sw) _) = s
pSegStart (PathRBez3 (s,sw) _ _) = s
pSegStart (PathEArc s _ _ _ _) = s

removeSndIf :: (a -> a -> Bool) -> [a] -> [a]
removeSndIf f [] = []
removeSndIf f [x] = [x]
removeSndIf f (x:y:xs) = if f x y then x:(removeSndIf f xs) else x:(removeSndIf f (y:xs))

data WholePathSegment 
  = WPathSeg !Segment
  | WPathBez2 !Bezier2
  | WPathBez3 !Bezier3
  | WPathRBez2 !RBezier2
  | WPathRBez3 !RBezier3
  | WPathEArc !EllipticalArc
  deriving (Show, Eq, Ord, Read)

instance Geometric WholePathSegment where 
  transform aff (WPathSeg seg) = WPathSeg (transform aff seg)
  transform aff (WPathBez2 seg) = WPathBez2 (transform aff seg)
  transform aff (WPathBez3 seg) = WPathBez3 (transform aff seg)
  transform aff (WPathRBez2 seg) = WPathRBez2 (transform aff seg)
  transform aff (WPathRBez3 seg) = WPathRBez3 (transform aff seg)
  transform aff (WPathEArc seg) = WPathEArc (transform aff seg)

-- replaces linear bezier curves with line segments,
-- and rational bezier curves with constant weights by normal bezier curves
-- EArcs and line segments are presently not simplifiable
-- tho we could remove line segments of length 0
-- actually hmmm for animation purposes, maybe better if we only subdivide, but just something to keep in mind
-- in case it actually comes up. This might work just fine.
--
-- the purpose of the simplification is for stroking, so the primary goal is removing segment internal cusps
-- and self intersections
simplifyWPSeg :: Double -> WholePathSegment -> ([PathSegment], Point)
simplifyWPSeg tol seg@(WPathBez2 bez) = 
  let
    s = start2 bez
    e = end2 bez
    xs@(a2,a1,a0) = xCoeffs2 bez
    computeX = evalQuadratic xs
    ys@(b2,b1,b0) = yCoeffs2 bez
    computeY = evalQuadratic ys
    xps@(c1,c0) = derivCoeffsQuadratic xs
    yps@(d1,d0) = derivCoeffsQuadratic ys
    -- ok, when is this linear/a cusp
    -- linear iff crss == 0 i.e. abs crss < tol
    crss = a2*b1-b2*a1
    -- need to figure out whether this can be replaced by one or two line segments
  in
    if abs crss < tol
    then
      if abs c1 < tol
      then
        if abs c0 < tol
        then
          if abs d1 < tol
          then
            if abs d0 < tol
            then
              -- both derivatives are constantly 0, so the bezier is a curve
              ([],s)
            else
              -- no cusp
              ([PathSeg s],e)
          else
            let
              t = -d0/d1
              tpt = makePoint (computeX t) (computeY t)
            in
              if 0 <= t && t <= 1 -- is cusp on the bezier curve?
              then
                ([PathSeg s,PathSeg tpt],e)
              else
                ([PathSeg s],e)
        else
          ([PathSeg s],e) -- no cusp, so simple line segment
      else
        let
          t = -c0/c1 -- this is where the cusp is
          -- turning point
          tpt = makePoint (computeX t) (computeY t)
        in
          if 0 <= t && t <= 1 -- is cusp on the bezier curve?
          then
            ([PathSeg s,PathSeg tpt],e)
          else
            ([PathSeg s],e)
    else
      toPathSpec seg
-- TODO: make this handle small loops and generally self intersections better,
-- tho this does detect cusps :)
-- but it doesn't detect linearity.
simplifyWPSeg tol seg@(WPathBez3 bez) = 
  let
    s = start3 bez
    e = end3 bez
    xs@(a3,a2,a1,a0) = xCoeffs3 bez
    computeX = evalCubic xs
    ys@(b3,b2,b1,b0) = yCoeffs3 bez
    computeY = evalCubic ys
    xps@(c2,c1,c0) = derivCoeffsCubic xs
    yps@(d2,d1,d0) = derivCoeffsCubic ys
    -- uhoh what abt repeated roots?? TODO
    inrange r t = -r <= t && t <= 1+r
    xprs = filter (inrange tol) $ solveQuadraticTol tol xps
    yprs = filter (inrange tol) $ solveQuadraticTol tol yps
    -- cusp candidates
    -- the filter is because we don't want to subdivide if the cusp is at the end of the interval, which is
    -- ok.
    cuspcs = filter (inrange (-tol)) [ (t1+t2)/2 | t1 <- xprs, t2 <- yprs, abs (t1-t2) < tol ]
    cusps = removeSndIf (\x y -> abs (x-y) < tol && y /= 1) $ 0:(L.sort (1:cuspcs))
  in
    if abs a3 < tol && abs b3 < tol -- this is a quadratic bezier in disguise, convert it and simplify
    then
      simplifyWPSeg tol (WPathBez2 (bezierFromCoeffs2 (a2,a1,a0) (b2,b1,b0)))
    else
      -- subdivide at cusp candidates
      -- toPathSpec seg
      if cusps == [0,1]
      then
        ([makeBez3Seg bez],e)
      else
        (map makeBez3Seg $ subdivideBezier3 cusps bez, e)
simplifyWPSeg tol seg@(WPathRBez2 rbez) = 
  let
    (s,sw) = rstart2 rbez
    (c,cw) = rcontrol2 rbez
    (e,ew) = rend2 rbez
  in
    if abs ((sw/ew)-1) < tol && abs ((cw/ew)-1) < tol
    then
      simplifyWPSeg tol . WPathBez2 $ makeBezier2 s c e
    else
      toPathSpec seg
simplifyWPSeg tol seg@(WPathRBez3 rbez) =
  let
    (s,sw) = rstart3 rbez
    (c,cw) = rstCont3 rbez
    (d,dw) = rendCont3 rbez
    (e,ew) = rend3 rbez
  in
    if abs ((sw/ew)-1) < tol && abs ((cw/ew)-1) < tol && abs ((dw/ew)-1) < tol
    then
      simplifyWPSeg tol . WPathBez3 $ makeBezier3 s c d e 
      -- would be better to detect cusps and stuff for rational beziers,
      -- but the derivatives are more complicated
    else
      toPathSpec seg
simplifyWPSeg _ seg = toPathSpec seg

toPathSpec :: WholePathSegment -> ([PathSegment],Point)
toPathSpec (WPathSeg seg) = ([PathSeg (seg^.start)], seg^.end)
toPathSpec (WPathBez2 bez) = ([PathBez2 (start2 bez) (control2 bez)], end2 bez)
toPathSpec (WPathBez3 bez) = ([PathBez3 (start3 bez) (stCont3 bez) (endCont3 bez)], end3 bez)
toPathSpec (WPathRBez2 rbez) = 
  let
    (s,sw) = rstart2 rbez
    (c,cw) = rcontrol2 rbez
    (e,ew) = rend2 rbez
  in
    ([PathRBez2 (s,sw/ew) (c,cw/ew)], e)
toPathSpec (WPathRBez3 rbez) = 
  let
    (s,sw) = rstart3 rbez
    (c,cw) = rstCont3 rbez
    (d,dw) = rendCont3 rbez
    (e,ew) = rend3 rbez
  in
    ([PathRBez3 (s,sw/ew) (c,cw/ew) (d,dw/ew)], e)
toPathSpec (WPathEArc earc) =
  let 
    (mat,tol) = earc^.ellipse.matrix
    deltphi = earc^.delta
    fA = abs deltphi >= pi
    fS = deltphi >= 0
  in
    ( [ PathEArc (parametrizeEArc earc 0) mat fA fS tol ]
    , (parametrizeEArc earc 1)
    )

evaluate :: WholePathSegment -> Double -> Point
evaluate (WPathSeg seg) = segmentParametrization seg
evaluate (WPathBez2 bez) = parametrization2 bez
evaluate (WPathBez3 bez) = parametrization3 bez
evaluate (WPathRBez2 rbez) = rparametrization2 rbez
evaluate (WPathRBez3 rbez) = rparametrization3 rbez
evaluate (WPathEArc earc) = parametrizeEArc earc

derivative :: WholePathSegment -> Double -> T.Vector
derivative (WPathSeg seg) _ = 
  (seg^.end)-.(seg^.start)
derivative (WPathBez2 bez) t = 
  let
    (a2,a1,_) = xCoeffs2 bez
    (b2,b1,_) = yCoeffs2 bez
  in
    makeVector (2*a2*t+a1) (2*b2*t+b1)
derivative (WPathBez3 bez) t = 
  let
    (a3,a2,a1,_) = xCoeffs3 bez
    (b3,b2,b1,_) = yCoeffs3 bez
  in
    makeVector (t*(t*3*a3+2*a2)+a1) (t*(t*3*b3+2*b2)+b1)
derivative (WPathRBez2 rbez) t = rderivative2 rbez t
derivative (WPathRBez3 rbez) t = rderivative3 rbez t
derivative (WPathEArc earc) t = derivativeEArc earc t

-- needed to properly deal with cusps
-- startTangent tol (WPathSeg seg) = normalize $ (seg^.start) -. (seg^.end)
startTangent :: Double -> WholePathSegment -> T.Vector
startTangent tol (WPathBez2 bez) = 
  let
    s = start2 bez
    c = control2 bez
    e = end2 bez
    vec1 = c-.s
    vec2 = e-.s
    l1 = vectorNorm vec1
  in 
    if l1 < tol
    then
      normalize  vec2
    else
      (1/l1)*.vec1
startTangent tol (WPathBez3 bez) = 
  let
    s = start3 bez
    c = stCont3 bez
    d = endCont3 bez
    e = end3 bez
    vec1 = c -. s
    vec2 = d -. s
    vec3 = e -. s
    l1 = vectorNorm vec1
    l2 = vectorNorm vec2
  in 
    if l1 < tol
    then
      if l2 < tol
      then
        normalize vec3
      else
        (1/l2)*.vec2
    else
      (1/l1)*.vec1
startTangent tol (WPathRBez2 rbez) = 
  let
    (s,_) = rstart2 rbez
    (c,_) = rcontrol2 rbez
    (e,_) = rend2 rbez
    vec1 = c-.s
    vec2 = e-.s
    l1 = vectorNorm vec1
  in 
    if l1 < tol
    then
      normalize $ vec2
    else
      (1/l1)*.vec1
startTangent tol (WPathRBez3 rbez) = 
  let
    (s,_) = rstart3 rbez
    (c,_) = rstCont3 rbez
    (d,_) = rendCont3 rbez
    (e,_) = rend3 rbez
    vec1 = c -. s
    vec2 = d -. s
    vec3 = e -. s
    l1 = vectorNorm vec1
    l2 = vectorNorm vec2
  in 
    if l1 < tol
    then
      if l2 < tol
      then
        normalize vec3
      else
        (1/l2)*.vec2
    else
      (1/l1)*.vec1
startTangent _ wps = pathTangent wps 0

endTangent :: Double -> WholePathSegment -> T.Vector
endTangent tol (WPathBez2 bez) = 
  let
    s = start2 bez
    c = control2 bez
    e = end2 bez
    vec1 = e-.c
    vec2 = e-.s
    l1 = vectorNorm vec1
  in 
    if l1 < tol
    then
      normalize  vec2
    else
      (1/l1)*.vec1
endTangent tol (WPathBez3 bez) = 
  let
    s = start3 bez
    c = stCont3 bez
    d = endCont3 bez
    e = end3 bez
    vec1 = e -. d
    vec2 = e -. c
    vec3 = e -. s
    l1 = vectorNorm vec1
    l2 = vectorNorm vec2
  in 
    if l1 < tol
    then
      if l2 < tol
      then
        normalize vec3
      else
        (1/l2)*.vec2
    else
      (1/l1)*.vec1
endTangent tol (WPathRBez2 rbez) = 
  let
    (s,_) = rstart2 rbez
    (c,_) = rcontrol2 rbez
    (e,_) = rend2 rbez
    vec1 = e-.c
    vec2 = e-.s
    l1 = vectorNorm vec1
  in 
    if l1 < tol
    then
      normalize $ vec2
    else
      (1/l1)*.vec1
endTangent tol (WPathRBez3 rbez) = 
  let
    (s,_) = rstart3 rbez
    (c,_) = rstCont3 rbez
    (d,_) = rendCont3 rbez
    (e,_) = rend3 rbez
    vec1 = e -. d
    vec2 = e -. c
    vec3 = e -. s
    l1 = vectorNorm vec1
    l2 = vectorNorm vec2
  in 
    if l1 < tol
    then
      if l2 < tol
      then
        normalize vec3
      else
        (1/l2)*.vec2
    else
      (1/l1)*.vec1
endTangent _ wps = pathTangent wps 1

pathTangent :: WholePathSegment -> Double -> T.Vector
pathTangent wpseg = normalize . derivative wpseg

pathNormal :: WholePathSegment -> Double -> T.Vector
pathNormal wpseg = perpendicular . pathTangent wpseg

startNormal :: Double -> WholePathSegment -> T.Vector
startNormal tol = perpendicular . startTangent tol

endNormal :: Double -> WholePathSegment -> T.Vector
endNormal tol = perpendicular . endTangent tol

reverseSegment :: PathSegment -> Point -> PathSegment
reverseSegment (PathSeg _) rs = PathSeg rs
reverseSegment (PathBez2 _ c) rs = PathBez2 rs c
reverseSegment (PathBez3 _ c d) rs = PathBez3 rs d c
reverseSegment (PathRBez2 (_,sw) (c,cw)) rs = PathRBez2 (rs,1/sw) (c,cw/sw)
reverseSegment (PathRBez3 (_,sw) (c,cw) (d,dw)) rs = PathRBez3 (rs,1/sw) (d,dw/sw) (c,cw/sw)
reverseSegment (PathEArc _ mat fA fS tol) rs = PathEArc rs mat fA (not fS) tol

wpSegBoundingBox :: WholePathSegment -> Box
wpSegBoundingBox (WPathSeg seg) = bounds seg
wpSegBoundingBox (WPathBez2 bez) = bounds bez
wpSegBoundingBox (WPathBez3 bez) = bounds bez
wpSegBoundingBox (WPathRBez2 rbez) = bounds rbez
wpSegBoundingBox (WPathRBez3 rbez) = bounds rbez
wpSegBoundingBox (WPathEArc earc) = bounds earc

instance GBounded WholePathSegment where
  bounds = wpSegBoundingBox

toWholeSeg :: PathSegment -> Point -> WholePathSegment
toWholeSeg (PathSeg p1) = WPathSeg . makeSegment p1
toWholeSeg (PathBez2 s c) = WPathBez2 . makeBezier2 s c
toWholeSeg (PathBez3 s c d) = WPathBez3 . makeBezier3 s c d
toWholeSeg (PathRBez2 s c) = WPathRBez2 . makeRBezier2 s c . (,1)
toWholeSeg (PathRBez3 s c d) = WPathRBez3 . makeRBezier3 s c d . (,1)
toWholeSeg (PathEArc s mat fA fS tol) = WPathEArc . makeEArcMatrixEndpoints s (mat,tol) fA fS

getVertexC :: Int -> Contour -> Point
getVertexC n Contour{contourSegs = cs} = pSegStart $ cs!(n `mod` (length cs))

getVertexP :: Int -> Path -> Point
getVertexP n Path{pathSegs = (ps,cap)} = 
  let
    l = length ps 
    nn = n `mod` (l+1)
  in
    if nn == l
    then
      cap
    else
      pSegStart $ ps ! nn

getWholeSegmentC :: Int -> Contour -> WholePathSegment
getWholeSegmentC n c@Contour{contourSegs = cs} = toWholeSeg (cs!n) (getVertexC (n+1) c)

getWholeSegmentP :: Int -> Path -> WholePathSegment
getWholeSegmentP n p@Path{pathSegs = (ps,_)} = toWholeSeg (ps!n) (getVertexP (n+1) p)

newtype Contour
  = Contour { contourSegs :: Vector PathSegment }
  deriving (Show, Eq, Ord, Read)

instance GBounded Contour where
  bounds = bounds . V.toList . toWholeSegsC

instance Geometric Contour where
  transform aff Contour{contourSegs = ps} = Contour $ fmap (transform aff) ps
 
instance (Functor f, Geometric a) => Geometric (f a) where
  transform aff = fmap (transform aff)

makeContour :: [PathSegment] -> Contour
makeContour = Contour . V.fromList

type PathSpec = ([PathSegment],Point)

newtype Path = Path { pathSegs :: (Vector PathSegment, Point) }
  deriving (Show, Eq, Ord, Read)

instance GBounded Path where
  bounds = bounds . V.toList . toWholeSegsP

instance Geometric Path where
  transform aff Path{pathSegs = (ps,cap)} = Path (fmap (transform aff) ps, transform aff cap)

makePath :: [PathSegment] -> Point -> Path
makePath ps cap = Path (V.fromList ps, cap)

pathFromSpec :: PathSpec -> Path
pathFromSpec = uncurry makePath

numSegsC :: Contour -> Int
numSegsC = V.length . contourSegs

numSegsP :: Path -> Int
numSegsP op =
  let
    (v,_) = pathSegs op
  in
    V.length v

-- isomorphic to nonempty really, maybe should use instead
-- I'm being lazy here
type ClosedPath = [Contour]

toWholeSegsC :: Contour -> Vector WholePathSegment
toWholeSegsC scp = 
  let
    psegs = contourSegs scp
    --n = numSegsC scp
  in
    --imap (\i _ -> getWholeSegmentC i scp) psegs
    imap (\i pseg -> toWholeSeg pseg (getVertexC (i+1) scp)) psegs

toWholeSegsP :: Path -> Vector WholePathSegment
toWholeSegsP path =
  let
    (psegs,cap) = pathSegs path
    --n = numSegsP path
  in
    --imap (\i _ -> getWholeSegmentP i path) psegs
    imap (\i pseg -> toWholeSeg pseg (getVertexP (i+1) path)) psegs


toWholeSegsCP :: ClosedPath -> Vector WholePathSegment
toWholeSegsCP = V.concat . map toWholeSegsC

toWholeSegsPredSucc :: ClosedPath -> Vector (WholePathSegment, (Int,Int))
toWholeSegsPredSucc = V.concat . flip toWholeSegsPSH 0

toWholeSegsPSH :: ClosedPath -> Int -> [Vector (WholePathSegment, (Int,Int))]
toWholeSegsPSH [] _ = []
toWholeSegsPSH (cont:cs) n = 
  let
    segs = toWholeSegsC cont
    l = V.length segs
    is = V.enumFromN 0 l
    ps = V.map ((+n) . (`mod` l) . (+(l-1))) is
    ss = V.map ((+n) . (`mod` l) . (+1)) is
  in
    (V.zip segs (V.zip ps ss)) : (toWholeSegsPSH cs (n+l))

cutContour :: Contour -> Path
cutContour Contour{contourSegs=cs} = Path (cs, pSegStart $ (cs!0))

-- assumes (without checking) that the start point and end point of the path are the same
-- joinPathUnsafe . cutContour = id
joinPathUnsafe :: Path -> Contour
joinPathUnsafe Path{pathSegs=(ps,_)} = Contour ps

-- uses joinPath, to ensure that the path is joined with a segment when the start and end points are 
-- not exactly equal, and is simply joined together otherwise, since there's no reason to add a zero length
-- segment
joinPathSegment :: Path -> Contour
joinPathSegment p = fromMaybe (joinPathSegUnsafe p) (joinPath p)

joinPathSegUnsafe :: Path -> Contour
joinPathSegUnsafe Path{pathSegs=(ps,cap)} = Contour (ps `V.snoc` (PathSeg cap))

-- requires the start and end points to be ***exactly*** equal
joinPath :: Path -> Maybe Contour
joinPath Path{pathSegs=(ps,cap)} = 
  if (pSegStart (ps!0)) == cap
  then
    Just $ Contour ps
  else
    Nothing

reverseP :: Path -> Path
reverseP Path{pathSegs=(ps,cap)} = Path (V.reverse $ V.imap f ps, (pSegStart $ (ps!0)))
  where
    l = V.length ps
    reverseStart i = if i == l-1 then cap else (pSegStart $ ps!(i+1))
    f i seg = reverseSegment seg (reverseStart i)

reverseC :: Contour -> Contour
reverseC Contour{contourSegs=cs} = Contour . V.reverse $ V.imap f cs
  where
    l = V.length cs
    reverseStart i = pSegStart $ cs!((i+1) `mod` l)
    f i seg = reverseSegment seg (reverseStart i)

--toWholeSegsOP :: Path -> Vector WholePathSegment

class Pathable a where
  toPath :: a -> Path
  pathStart :: a -> Point
  pathStart = pathStart . toPath
  pathEnd :: a -> Point
  pathEnd = pathEnd . toPath

instance Pathable Path where
  toPath = id
  pathStart Path{pathSegs=(ps,cap)} = 
    if V.length ps == 0
    then
      cap
    else
      pSegStart $ (ps!0)
  pathEnd Path{pathSegs=(_,cap)} = cap

instance Pathable ([PathSegment],Point) where
  toPath = pathFromSpec
  pathStart ([],cap) = cap
  pathStart (seg:_,_) = pSegStart seg
  pathEnd (_,cap) = cap


instance Pathable Bezier2 where
  toPath bez = Path (V.singleton $ PathBez2 (start2 bez) (control2 bez),end2 bez)
  pathStart = start2
  pathEnd = end2

instance Pathable RBezier2 where
  toPath rbez = 
    let 
      (s,sw) = rstart2 rbez
      (c,cw) = rcontrol2 rbez
      (e,ew) = rend2 rbez
    in
      Path (V.singleton $ PathRBez2 (s,sw/ew) (c,cw/ew),e)
  pathStart = fst . rstart2
  pathEnd = fst . rend2

instance Pathable Bezier3 where
  toPath bez = Path (V.singleton $ PathBez3 (start3 bez) (stCont3 bez) (endCont3 bez),end3 bez)
  pathStart = start3
  pathEnd = end3

instance Pathable RBezier3 where
  toPath rbez = 
    let 
      (s,sw) = rstart3 rbez
      (c,cw) = rstCont3 rbez
      (d,dw) = rendCont3 rbez
      (e,ew) = rend3 rbez
    in
      Path (V.singleton $ PathRBez3 (s,sw/ew) (c,cw/ew) (d,dw/ew),e)
  pathStart = fst . rstart3
  pathEnd = fst . rend3

