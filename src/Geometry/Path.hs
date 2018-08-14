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
  | PathEArc !Point !Double !Double !Double !Bool !Bool
  deriving (Show, Eq, Ord, Read)

instance Geometric PathSegment where
  transform aff (PathSeg pt) = PathSeg (transform aff pt)
  transform aff (PathBez2 s c) = PathBez2 (transform aff s) (transform aff c)
  transform aff (PathBez3 s c d) = PathBez3 (transform aff s) (transform aff c) (transform aff d)

pSegStart :: PathSegment -> Point
pSegStart (PathSeg s) = s
pSegStart (PathBez2 s _) = s
pSegStart (PathBez3 s _ _) = s


data WholePathSegment 
  = WPathSeg !Segment
  | WPathBez2 !Bezier2
  | WPathBez3 !Bezier3
  deriving (Show, Eq, Ord, Read)

instance Geometric WholePathSegment where 
  transform aff (WPathSeg seg) = WPathSeg (transform aff seg)
  transform aff (WPathBez2 seg) = WPathBez2 (transform aff seg)
  transform aff (WPathBez3 seg) = WPathBez3 (transform aff seg)

evaluate :: WholePathSegment -> Double -> Point
evaluate (WPathSeg seg) = segmentParametrization seg
evaluate (WPathBez2 bez) = parametrization2 bez
evaluate (WPathBez3 bez) = parametrization3 bez

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

pathTangent :: WholePathSegment -> Double -> T.Vector
pathTangent wpseg = normalize . derivative wpseg

pathNormal :: WholePathSegment -> Double -> T.Vector
pathNormal wpseg = perpendicular . pathTangent wpseg


wpSegBoundingBox :: WholePathSegment -> Box
wpSegBoundingBox (WPathSeg seg) = bounds seg
wpSegBoundingBox (WPathBez2 bez) = bounds bez
wpSegBoundingBox (WPathBez3 bez) = bounds bez

instance GBounded WholePathSegment where
  bounds = wpSegBoundingBox

toWholeSeg :: PathSegment -> Point -> WholePathSegment
toWholeSeg (PathSeg p1) = WPathSeg . makeSegment p1
toWholeSeg (PathBez2 s c) = WPathBez2 . makeBezier2 s c
toWholeSeg (PathBez3 s c d) = WPathBez3 . makeBezier3 s c d

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
    f i (PathSeg _) = PathSeg (reverseStart i)
    f i (PathBez2 _ c) = PathBez2 (reverseStart i) c
    f i (PathBez3 _ c d) = PathBez3 (reverseStart i) d c

reverseC :: Contour -> Contour
reverseC Contour{contourSegs=cs} = Contour . V.reverse $ V.imap f cs
  where
    l = V.length cs
    reverseStart i = pSegStart $ cs!((i+1) `mod` l)
    f i (PathSeg _) = PathSeg (reverseStart i)
    f i (PathBez2 _ c) = PathBez2 (reverseStart i) c
    f i (PathBez3 _ c d) = PathBez3 (reverseStart i) d c

--toWholeSegsOP :: Path -> Vector WholePathSegment

