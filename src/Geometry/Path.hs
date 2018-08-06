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

import qualified Data.Vector as V
import Data.Vector ((!),imap,Vector)
--import Data.List.Nonempty


data PathSegment
  = PathSeg !Point
  | PathBez2 !Point !Point
  | PathBez3 !Point !Point !Point
  deriving (Show, Eq, Ord, Read)

pSegStart :: PathSegment -> Point
pSegStart (PathSeg s) = s
pSegStart (PathBez2 s _) = s
pSegStart (PathBez3 s _ _) = s

data WholePathSegment 
  = WPathSeg !Segment
  | WPathBez2 !Bezier2
  | WPathBez3 !Bezier3
  deriving (Show, Eq, Ord, Read)

toWholeSeg :: PathSegment -> Point -> WholePathSegment
toWholeSeg (PathSeg p1) = WPathSeg . makeSegment p1
toWholeSeg (PathBez2 s c) = WPathBez2 . makeBezier2 s c
toWholeSeg (PathBez3 s c d) = WPathBez3 . makeBezier3 s c d

newtype SimpleClosedPath
  = SCPath { pathSegs :: Vector PathSegment }
  deriving (Show, Eq, Ord, Read)

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



