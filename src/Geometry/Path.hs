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

import Control.Lens ((^.))
--import Data.List.Nonempty


data PathSegment
  = PathSeg !Point
  | PathBez2 !Point !Point
  | PathBez3 !Point !Point !Point
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

newtype SimpleClosedPath
  = SCPath { pathSegs :: Vector PathSegment }
  deriving (Show, Eq, Ord, Read)

instance GBounded SimpleClosedPath where
  bounds = bounds . V.toList . toWholeSegsSCP

instance Geometric SimpleClosedPath where
  transform aff SCPath{pathSegs = ps} = SCPath $ fmap (transform aff) ps
 
instance (Functor f, Geometric a) => Geometric (f a) where
  transform aff = fmap (transform aff)

makeSCP :: [PathSegment] -> SimpleClosedPath
makeSCP = SCPath . V.fromList

newtype OpenPath = OPath { opathSegs :: (Vector PathSegment, Point) }
  deriving (Show, Eq, Ord, Read)

numSegsSCP :: SimpleClosedPath -> Int
numSegsSCP = V.length . pathSegs

numSegsOP :: OpenPath -> Int
numSegsOP op =
  let
    (v,_) = opathSegs op
  in
    V.length v

-- isomorphic to nonempty really, maybe should use instead
-- I'm being lazy here
type ClosedPath = [SimpleClosedPath]

toWholeSegsSCP :: SimpleClosedPath -> Vector WholePathSegment
toWholeSegsSCP scp = 
  let
    psegs = pathSegs scp
    n = numSegsSCP scp
  in
    imap (\i pseg -> toWholeSeg pseg (pSegStart (psegs!((i+1) `mod` n)))) psegs

toWholeSegsCP :: ClosedPath -> Vector WholePathSegment
toWholeSegsCP = V.concat . map toWholeSegsSCP

--toWholeSegsOP :: OpenPath -> Vector WholePathSegment

