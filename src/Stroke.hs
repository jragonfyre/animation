--
-- Stroke.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Stroke
  ( module Stroke
  ) where

import Geometry.Types
import Geometry.Affine
import Geometry.Bezier
import Geometry.Path

import qualified Data.Vector as V
import Data.Vector ((!))

data JoinType 
  = MiterJoin Double 
  -- Miter 0 is a sharp bend, and Miter 1 is a flat bend passing through the corner of the path
  -- jk the double will be the limit on the miter length. The miter can get arbitrarily long as we approach a
  -- cusp, so the double should cap it.
  -- jk, not quite sure what scaling to use... this is a pain
  -- maybe use SVG as a reference? uh duh. good idea self.
  | BevelJoin Double
  | BezierJoin Double -- essentially a miter join, but capped by a quadratic bezier instead of a sharp point
-- 
--  | ArcJoin Double 
  -- joins gaps between segments with a quadratic bezier with control point at the intersection of
  -- the pushed out ends of the control polygon.
  -- The double affects the size of the arc. At 0 it is a sharp bend. Not quite sure what scaling to use.
  --  | CircleJoin -- TODO add circle arcs, then we can join with circles
  deriving (Show, Eq, Ord, Read)

data CapType
  = PointCap Double -- Double -- The first double is the aspect ratio of the caps height to half-width
  -- (0 is flat, 1 is 90 degree angle at the point, so height is equal to half the width), the
  -- second is the distance of the cap from the end of the path, currently not using the second
  --  | CircleCap
  | FlatCap Double -- capped by a flat segment. 0.0 means the cap passes through the end of the path, 1.0
  -- means that the flat cap is strokeDistance away from the end of the path
  -- (That's SVG's square cap)
  deriving (Show, Eq, Ord, Read)

buttCap :: CapType
buttCap = FlatCap 0

squareCap :: CapType
squareCap = FlatCap 1

data StrokeStyle = StrokeStyle
  { strokeDistance :: Double
  , joinType :: JoinType
  , capType :: CapType
  }
  deriving (Show, Eq, Ord, Read)

--(v`dot` norm1)=d
--(v`dot` norm2)=d

lPathStart :: ([PathSegment],Point) -> Point
lPathStart (seg:_,_) = pSegStart seg
lPathStart ([],cap) = cap

lPathEnd :: ([PathSegment],Point) -> Point
lPathEnd = snd

lPathSegs :: ([PathSegment],Point) -> [PathSegment]
lPathSegs = fst

-- tang is the incoming tangent vector to the path end or cusp
buildCap :: StrokeStyle -> Vector -> Point -> ([PathSegment],Point)
buildCap ss tang center =
  let
    dist = strokeDistance ss
    cap = capType ss
    norm1 = perpendicular tang
    norm2 = perpendicularLeft tang
    vec1 = dist*.norm1
    vec2 = dist*.norm2
    p1 = center +. vec1
    p2 = center +. vec2
  in
    case cap of
      PointCap ar ->
        ([PathSeg p1, PathSeg (center +. ((ar*dist)*.tang))],p2)
      FlatCap ar ->
        if ar == 0
        then
          ([PathSeg p1],p2)
        else
          let
            v=(ar*dist)*.tang
          in
            ([PathSeg p1, PathSeg (p1+.v), PathSeg (p2+.v)],p2)

    

buildJoin :: StrokeStyle -> Vector -> Vector -> Point -> ([PathSegment],Point)
buildJoin ss norm1 norm2 center = 
  let
    dist = strokeDistance ss
    join = joinType ss
    --cap = capType ss
    mat = transpose $ makeMatrix norm1 norm2
    c = det mat -- norm1 `cross` norm2 
    -- (these are equal anyway by definition, so might as well cut out a step)
    inv = invertMatrix mat
    --d = norm1 `dot` norm2
    vec1 = dist*.norm1
    vec2 = dist*.norm2
    p1 = center +. vec1
    p2 = center +. vec2
  in 
    if c==0 -- norm1 and norm2 are either parallel (join is easy) or antiparallel (cusp, oh shit)
    then
      if norm1 == norm2 -- parallel join, no need for any joining
      then 
        ([], p1)
      else -- antiparallel, i.e. cusp, right now I'll join with a segment
        buildCap ss (perpendicularLeft norm1) center
        --([PathSeg p1], p2)
    else
      let 
        cVec = inv *. (makeVector dist dist)
        -- find a vector whose dot with both norm1 and norm2 is dist, this is the 
        -- vector from center to the intersection of the lines with normals norm1 and norm2 pushed out along
        -- their normals a distance dist.
        pc = center +. cVec
      in
        if c > 0 -- norm2 is within +180 degrees of norm1, so this is an exterior join
        then
          case join of
            BevelJoin s -> 
              ([PathSeg p1], p2)
            MiterJoin s -> -- ignore scaling for now.
              ([PathSeg p1, PathSeg pc],p2)
              --([], pc)
            BezierJoin s -> -- ignore scaling for now
              ([PathBez2 p1 pc], p2)
        else -- interior join
          ([], pc)
      
-- need to refactor the line intersection code. DRY!!!! It's really badly repeated rn
-- takes the initial segment and the start and end of the desired new parallel segment
-- as determined by buildJoin
-- plus the distance 
parallelSegment :: Double -> WholePathSegment -> Point -> Point -> [PathSegment]
parallelSegment _ (WPathSeg _) s _ = [PathSeg s]
parallelSegment d (WPathBez2 bez) s e =
  let
    ps = start2 bez
    pc = control2 bez
    pe = end2 bez
    norm1 = perpendicular . normalize $ pc -. ps
    norm2 = perpendicular . normalize $ pe -. pc
    mat = transpose $ makeMatrix norm1 norm2
    c = det mat -- norm1 `cross` norm2 
    -- (these are equal anyway by definition, so might as well cut out a step)
    inv = invertMatrix mat
    cVec = 
      if c /= 0
      then inv *. (makeVector d d)
      else
        if norm1 == norm2
        then d *. norm1
        else d *. (perpendicular norm2)
          -- this *really* doesn't work near a cusp xP
  in
    [PathBez2 s (pc+.cVec)]
parallelSegment d (WPathBez3 bez) s e =
  let
    ps = start3 bez
    pc = stCont3 bez
    pd = endCont3 bez
    pe = end3 bez
    norm1 = perpendicular . normalize $ pc -. ps
    norm2 = perpendicular . normalize $ pd -. pc
    norm3 = perpendicular . normalize $ pe -. pd
    dVec = makeVector d d
    mat1 = transpose $ makeMatrix norm1 norm2
    c1 = det mat1 -- norm1 `cross` norm2 
    inv1 = invertMatrix mat1
    cVec1 = 
      if c1 /= 0
      then inv1 *. dVec
      else
        if norm1 == norm2
        then d *. norm1
        else d *. (perpendicular norm2)
          -- this *really* doesn't work near a cusp xP again
    mat2 = transpose $ makeMatrix norm2 norm3
    c2 = det mat2 -- norm2 `cross` norm3 
    inv2 = invertMatrix mat2
    cVec2 = 
      if c2 /= 0
      then inv2 *. dVec
      else
        if norm2 == norm3
        then d *. norm2
        else d *. (perpendicular norm3)
          -- this *really* doesn't work near a cusp xP still xP
  in
    [PathBez3 s (pc+.cVec1) (pd+.cVec2)]

-- IT WORKS!!!
strokeExterior :: StrokeStyle -> Contour -> Contour
strokeExterior ss@StrokeStyle{strokeDistance = dist} cont@Contour{contourSegs=cs} = 
  let
    --csegL = V.toList cs
    ws = toWholeSegsC cont
    n = V.length cs
    starts = fmap pSegStart cs
    ends = V.imap (\i _ -> pSegStart $ (cs!((i+1)`mod`n))) cs
    normal1s = V.map (\wps -> pathNormal wps 1) ws
    normal2s = V.imap (\i _ -> pathNormal (ws!((i+1)`mod`n)) 0) ws
    joins = V.zipWith3 (buildJoin ss) normal1s normal2s ends
    joinSts = V.map lPathStart joins
    joinSegs = V.map lPathSegs joins
    joinEnds = V.map lPathEnd joins
    joinEndsRot = V.imap (\i _ -> joinEnds!((i-1)`mod`n)) joinEnds
    parSegs = V.zipWith3 (parallelSegment dist) ws joinEndsRot joinSts
    allSegs = V.zipWith (++) parSegs joinSegs
    strokecs = concat allSegs
  in 
    makeContour strokecs

strokeTestS :: StrokeStyle
strokeTestS = StrokeStyle 1 (BezierJoin 0) (squareCap)

strokeTestC :: Contour
strokeTestC = makeContour 
  [ PathSeg (makePoint 10 10) 
  , PathBez2 (makePoint 110 10) (makePoint 60 110)
  ]

strokeTestC3 :: Contour
strokeTestC3 = makeContour
  [ PathSeg (makePoint 10 10)
  , PathBez3 (makePoint 110 10) (makePoint 10 110) (makePoint 110 110)
  ]


-- stroke returns the inner and exterior contours (in that order)
-- there are more efficient ways to do this, which should maybe be written later, but this has the benefit
-- of being DRY, so *shrug*
strokeContour :: StrokeStyle -> Contour -> (Contour, Contour)
strokeContour s c = (strokeExterior s $ reverseC c, strokeExterior s c)

-- easy modification of strokeExterior
-- This all needs refactoring so I DRM xP don't repeat myself
-- but do the modification first!
-- think I need to somehow unify paths with contours.
-- they behave basically the same for a lot of applications
strokePathExterior :: StrokeStyle -> Path -> Path
strokePathExterior ss@StrokeStyle{strokeDistance = dist} path@Path{pathSegs=(ps,cap)} = 
  let
    --csegL = V.toList cs
    ws = toWholeSegsP path
    n = V.length ps
    begCap = buildCap ss (
    endCap = 
    starts = fmap pSegStart cs
    ends = V.imap (\i _ -> pSegStart $ (cs!((i+1)`mod`n))) cs
    normal1s = V.map (\wps -> pathNormal wps 1) ws
    normal2s = V.imap (\i _ -> pathNormal (ws!((i+1)`mod`n)) 0) ws
    joins = V.zipWith3 (buildJoin ss) normal1s normal2s ends
    joinSts = V.map lPathStart joins
    joinSegs = V.map lPathSegs joins
    joinEnds = V.map lPathEnd joins
    joinEndsRot = V.imap (\i _ -> joinEnds!((i-1)`mod`n)) joinEnds
    parSegs = V.zipWith3 (parallelSegment dist) ws joinEndsRot joinSts
    allSegs = V.zipWith (++) parSegs joinSegs
    strokecs = concat allSegs
  in 
    makeContour strokecs

-- strokes the path
strokePath :: StrokeStyle -> Path -> Contour
strokePath ss p =
  let
    Path{pathSegs=(ps1,_)} = strokePathExterior ss p
    Path{pathSegs=(ps2,_)} = strokePathExterior ss $ reverseP p
  in
    Contour $ (V.++) ps1 ps2

