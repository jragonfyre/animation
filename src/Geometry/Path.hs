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

import qualified Data.Vector as V
import Data.Vector ((!),imap,Vector)

import Data.Maybe (fromMaybe)

import Control.Lens ((^.))
--import Data.List.Nonempty


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

pathTangent :: WholePathSegment -> Double -> T.Vector
pathTangent wpseg = normalize . derivative wpseg

pathNormal :: WholePathSegment -> Double -> T.Vector
pathNormal wpseg = perpendicular . pathTangent wpseg

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

newtype Path = Path { pathSegs :: (Vector PathSegment, Point) }
  deriving (Show, Eq, Ord, Read)

instance GBounded Path where
  bounds = bounds . V.toList . toWholeSegsP

instance Geometric Path where
  transform aff Path{pathSegs = (ps,cap)} = Path (fmap (transform aff) ps, transform aff cap)

makePath :: [PathSegment] -> Point -> Path
makePath ps cap = Path (V.fromList ps, cap)

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

