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
import Geometry.Ellipse
import Geometry.Path

import Control.Lens ((^.))

import qualified Data.Vector as V
import Data.Vector ((!))

-- | Represents a method of joining our new parallel 'PathSegment's at bends when stroking a 'Path'.
data JoinType 
  -- | A 'MiterJoin' is created by extending the segments in straight lines in the tangent direction at
  --   the end of the segment until they intersect forming a 
  --   sharp point. 
  = MiterJoin -- Double 
  -- Miter 0 is a sharp bend, and Miter 1 is a flat bend passing through the corner of the path
  -- jk the double will be the limit on the miter length. The miter can get arbitrarily long as we approach a
  -- cusp, so the double should cap it.
  -- jk, not quite sure what scaling to use... this is a pain
  -- maybe use SVG as a reference? uh duh. good idea self.
  -- | Connects the two end points of the segments to be joined by a straight line.
  | BevelJoin -- Double
  -- | Uses the same three points as a 'MiterJoin', but uses the point of the 'MiterJoin' as the control point
  --   of a 'Bezier2'.
  | BezierJoin -- Double -- essentially a miter join, but capped by a quadratic bezier instead of a sharp point
  -- | Joins the end points of the parallel segments 
  --   with a circular arc centered at the original join 'Point's of the segments in the original 'Path'
  | RoundJoin -- joins with a circle
-- 
--  | ArcJoin Double 
  -- joins gaps between segments with a quadratic bezier with control point at the intersection of
  -- the pushed out ends of the control polygon.
  -- The double affects the size of the arc. At 0 it is a sharp bend. Not quite sure what scaling to use.
  --  | CircleJoin -- TODO add circle arcs, then we can join with circles
  deriving (Show, Eq, Ord, Read)

-- | Represents a way of capping paths at either end points or cusps (where joins typically fail).
--   It needs to connect points on the opposite sides of a circle centered at the end point or cusp.
data CapType
  -- | A pointy cap with aspect ratio (cap height to half-width) given by the 'Double' argument. 
  --   (0 is flat, 1 is 90 degree angle at the point, so height is equal to half the width)
  = PointCap Double -- Double -- The first double is the aspect ratio of the caps height to half-width
  -- (0 is flat, 1 is 90 degree angle at the point, so height is equal to half the width), the
  -- second is the distance of the cap from the end of the path, currently not using the second
  --  | CircleCap
  -- | Produces a cap by drawing a perpendicular segment some distance away from the path, specified by the
  --   double parameter. Again the parameter is the ratio of the height to the half-width. 
  --   Thus @0.0@ means the cap passes through the end of the path (SVG's butt cap),
  --   and @1.0@ gives a flat cap half the width of the cap away from the end of the path, which is 
  --   SVG's square cap. Synonyms for these special cases are provided as 'buttCap' and 'squareCap'.
  | FlatCap Double -- capped by a flat segment. 0.0 means the cap passes through the end of the path, 1.0
  -- means that the flat cap is strokeDistance away from the end of the path
  -- (That's SVG's square cap)
  -- | Produces a semicircular arc to cap the path.
  | RoundCap
  deriving (Show, Eq, Ord, Read)

-- | @'FlatCap' 0@, see 'CapType'.
buttCap :: CapType
buttCap = FlatCap 0

-- | @'FlatCap' 1@, see 'CapType'.
squareCap :: CapType
squareCap = FlatCap 1

-- | Defines the style for stroking 'Path's and 'Contour's.
data StrokeStyle = StrokeStyle
  { strokeDistance :: Double
  -- ^ the distance of the stroke from the path, i.e. half the width of the stroke
  , joinType :: JoinType
  -- ^ the type of join to use when needed
  , capType :: CapType
  -- ^ the type of cap to use when needed
  , ellipseTolerance :: Double
  -- ^ tolerance for converting ellipse matrices to radii
  , joinTolerance :: Double
  -- ^ tolerance for determining whether a join is parallel (no join needed), or antiparallel 
  --   (i.e. a cusp, so a cap is needed)
  , tangentTolerance :: Double
  -- ^ tolerance for determining when the derivative at a point is zero, so we can compute the proper tangent
  --   with fallback methods
  }
  deriving (Show, Eq, Ord, Read)


-- | See 'PathSpec'.
--   This basically just needs to be deleted, part of refactoring to be done.
--   TODO.
lPathStart :: ([PathSegment],Point) -> Point
lPathStart (seg:_,_) = pSegStart seg
lPathStart ([],cap) = cap

-- | See 'PathSpec'.
--   This basically just needs to be deleted, part of refactoring to be done.
--   TODO.
lPathEnd :: ([PathSegment],Point) -> Point
lPathEnd = snd

-- | See 'PathSpec'.
--   This basically just needs to be deleted, part of refactoring to be done.
--   TODO.
lPathSegs :: ([PathSegment],Point) -> [PathSegment]
lPathSegs = fst

-- tang is the incoming tangent vector to the path end or cusp
-- | Builds a 'PathSpec' for a cap using a particular 'StrokeStyle'.
buildCap :: StrokeStyle -- ^ style to build the cap in (uses 'capType' and 'strokeDistance')
         -> Vector -- ^ the tangent vector at the end of the incoming segment to the cap
         -> Point -- ^ the center of the cap
         -> ([PathSegment],Point)
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
      RoundCap ->
        ([makeCircleSegment p1 dist True (ellipseTolerance ss)],p2)

-- | Builds a 'PathSpec' for a join using a particular stroke style
buildJoin :: StrokeStyle -- ^ style to build the join in
                         -- (uses 'joinType', 'strokeDistance', and 'joinTolerance')
          -> Vector -- ^ path normal at the end of the incoming path segment
          -> Vector -- ^ path normal at the start of the outgoing path segment
          -> Point -- ^ intersection point of the incoming and outgoing path segment
          -> ([PathSegment],Point)
buildJoin ss norm1 norm2 center = 
  let
    tol = joinTolerance ss
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
    if abs c < tol -- norm1 and norm2 are either parallel (join is easy) or antiparallel (cusp, oh shit)
    then
      if vectorNorm (norm1 -. norm2) < tol -- parallel join, no need for any joining
      then 
        ([], p1)
      else -- antiparallel, i.e. cusp, right now I'll join with the current cap style
        buildCap ss (perpendicularLeft norm1) center
        --([PathSeg p1], p2) used to be a segment
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
            BevelJoin -> -- s -> -- ignore scaling for now
              ([PathSeg p1], p2)
            MiterJoin -> --s -> -- ignore scaling for now.
              ([PathSeg p1, PathSeg pc],p2)
              --([], pc)
            BezierJoin -> -- s -> -- ignore scaling for now
              ([PathBez2 p1 pc], p2)
            RoundJoin -> 
              ([makeCircleSegment p1 dist True (ellipseTolerance ss)],p2)
        else -- interior join
          ([], pc)
      
-- need to refactor the line intersection code. DRY!!!! It's really badly repeated rn
-- takes the initial segment and the start and end of the desired new parallel segment
-- as determined by buildJoin
-- plus the distance 
-- | Computes an approximate parallel path to a given 'WholePathSegment'
parallelSegment :: StrokeStyle -- ^ style to use to compute the parallel segment
                               -- (uses 'strokeDistance', 'tangentTolerance')
                -> WholePathSegment -- ^ the path segment to compute a parallel path to
                                    -- (in the normal direction of course)
                -> Point -- ^ start of the new parallel path (probably produced by 'buildCap' or 'buildJoin')
                -> Point -- ^ end  of the new parallel path (probably produced by 'buildCap' or 'buildJoin')
                -> [PathSegment]
parallelSegment _ (WPathSeg _) s _ = [PathSeg s]
parallelSegment ss seg@(WPathBez2 bez) s _ =
  let
    d = strokeDistance ss
    ttol = tangentTolerance ss
    ps = start2 bez
    pc = control2 bez
    pe = end2 bez
    norm1 = startNormal ttol seg --perpendicular . normalize $ pc -. ps
    norm2 = endNormal ttol seg --perpendicular . normalize $ pe -. pc
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
parallelSegment ss seg@(WPathBez3 bez) s _ =
  let
    d = strokeDistance ss
    ttol = tangentTolerance ss
    ps = start3 bez
    pc = stCont3 bez
    pd = endCont3 bez
    pe = end3 bez
    --norm1 = perpendicular . normalize $ pc -. ps
    --norm3 = perpendicular . normalize $ pe -. pd
    norm1 = startNormal ttol seg
    norm3 = endNormal ttol seg
    norm2 =
      let
        nvec2 =  pd -. pc
        l2 = vectorNorm nvec2
      in
        if l2 < ttol
        then
          (rotate ((angleFrom norm1 norm3)/2)) *. norm1
        else
          perpendicular $ (1/l2) *. nvec2
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

-- THIS IS SOOOO SKETCHY XD TODO
parallelSegment ss seg@(WPathRBez2 rbez) s _ =
  let
    d = strokeDistance ss
    ttol = tangentTolerance ss
    (ps,sw) = rstart2 rbez
    (pc,cw) = rcontrol2 rbez
    (pe,ew) = rend2 rbez
    norm1 = startNormal ttol seg --perpendicular . normalize $ pc -. ps
    norm2 = endNormal ttol seg --perpendicular . normalize $ pe -. pc
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
    [PathRBez2 (s,sw/ew) (pc+.cVec,cw/ew)]
-- THIS IS SOOOO SKETCHY XD TODO
parallelSegment ss seg@(WPathRBez3 rbez) s _ =
  let
    d = strokeDistance ss
    ttol = tangentTolerance ss
    (ps,sw) = rstart3 rbez
    (pc,cw) = rstCont3 rbez
    (pd,dw) = rendCont3 rbez
    (pe,ew) = rend3 rbez
    norm1 = startNormal ttol seg
    norm3 = endNormal ttol seg
    --norm2 = perpendicular . normalize $ pd -. pc -- uhoh, this can't possibly be good... TODO fix
    norm2 =
      let
        nvec2 =  pd -. pc 
        l2 = vectorNorm nvec2
      in
        if l2 < ttol
        then
          (rotate ((angleFrom norm1 norm3)/2)) *. norm1
        else
          perpendicular $ (1/l2) *. nvec2
    --norm1 = perpendicular . normalize $ pc -. ps
    --norm2 = perpendicular . normalize $ pd -. pc
    --norm3 = perpendicular . normalize $ pe -. pd
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
    [PathRBez3 (s,sw/ew) (pc+.cVec1,cw/ew) (pd+.cVec2,dw/ew)]
-- lazy parallel elliptical arc
parallelSegment ss (WPathEArc earc) s _ = 
  let
    d = strokeDistance ss
    ell = earc^.ellipse
    (_,tol) = ell^.matrix
    rx = ell^.radX
    ry = ell^.radY
    rphi = ell^.phi
    --cent = ell^.center
    delt = earc^.delta
    dist = if delt >= 0 then d else (-d)
  in
    [PathEArc s (ellipseRadiiAndRotationToMatrix (rx+dist,ry+dist,rphi)) (abs delt >= pi) (delt >= 0) tol]



-- IT WORKS!!!
-- | strokes the exterior of a 'Contour'
strokeExterior :: StrokeStyle -- ^ style to stroke the 'Contour'\'s exterior in
               -> Contour -- ^ the contour to stroke
               -> Contour
strokeExterior ss cont@Contour{contourSegs=cs} = 
  let
    --csegL = V.toList cs
    ttol = tangentTolerance ss
    ws = toWholeSegsC cont
    n = V.length cs
    --starts = fmap pSegStart cs
    ends = V.imap (\i _ -> pSegStart $ (cs!((i+1)`mod`n))) cs
    normal1s = V.map (\wps -> endNormal ttol wps) ws
    -- normal1s = V.map (\wps -> pathNormal wps 1) ws
    normal2s = V.imap (\i _ -> startNormal ttol (ws!((i+1)`mod`n))) ws
    -- normal2s = V.imap (\i _ -> pathNormal (ws!((i+1)`mod`n)) 0) ws
    joins = V.zipWith3 (buildJoin ss) normal1s normal2s ends
    joinSts = V.map lPathStart joins
    joinSegs = V.map lPathSegs joins
    joinEnds = V.map lPathEnd joins
    joinEndsRot = V.imap (\i _ -> joinEnds!((i-1)`mod`n)) joinEnds
    parSegs = V.zipWith3 (parallelSegment ss) ws joinEndsRot joinSts
    allSegs = V.zipWith (++) parSegs joinSegs
    strokecs = concat allSegs
  in 
    makeContour strokecs

-- | A default 'StrokeStyle' for use in tests.
strokeTestS :: StrokeStyle
strokeTestS = StrokeStyle 1 (RoundJoin) (squareCap) 1e-7 1e-7 1e-7

-- | A test 'Contour' for testing stroking 'Bezier2's
strokeTestC :: Contour
strokeTestC = makeContour 
  [ PathSeg (makePoint 10 10) 
  , PathBez2 (makePoint 110 10) (makePoint 60 110)
  ]

-- | A test 'Contour' for testing stroking 'Bezier3's
strokeTestC3 :: Contour
strokeTestC3 = makeContour
  [ PathSeg (makePoint 10 10)
  , PathBez3 (makePoint 110 10) (makePoint 10 110) (makePoint 110 110)
  ]


-- stroke returns the inner and exterior contours (in that order)
-- there are more efficient ways to do this, which should maybe be written later, but this has the benefit
-- of being DRY, so *shrug*
-- | Returns the inner and exterior 'Contour's of the stroke of a 'Contour'. Inner contour computed by
--   computing the exterior contour of the reverse of the original contour.
strokeContour :: StrokeStyle -- ^ style to stroke the 'Contour' with
              -> Contour -- ^ the contour to stroke
              -> (Contour, Contour) -- ^ @(innerContour,exteriorContour)@
strokeContour s c = (strokeExterior s $ reverseC c, strokeExterior s c)

-- easy modification of strokeExterior
-- This all needs refactoring so I DRM xP don't repeat myself
-- but do the modification first!
-- think I need to somehow unify paths with contours.
-- they behave basically the same for a lot of applications
-- | Strokes the \"exterior\" of a 'Path', where by exterior we mean produces the half of the stroke between
--   the caps in the normal direction.
strokePathExterior :: StrokeStyle -- ^ style to stroke the 'Path' with.
                   -> Point -- ^ start of the stroke's path, produced by building the caps of the stroke first.
                   -> Point -- ^ end of the stroke's path
                   -> Path -- ^ the path to stroke
                   -> Path
strokePathExterior ss st end path@Path{pathSegs=(ps,_)} = 
  let
    --csegL = V.toList cs
    ttol = tangentTolerance ss
    ws = toWholeSegsP path
    n = V.length ps
    ints = V.generate (n-1) $ flip getVertexP path . (+1)
    normal1s = V.generate (n-1) $ (\i -> endNormal ttol (ws!i))
    --normal1s = V.generate (n-1) $ (\i -> pathNormal (ws!i) 1)
    normal2s = V.generate (n-1) $ (\i -> startNormal ttol (ws!(i+1)))
    --normal2s = V.generate (n-1) $ (\i -> pathNormal (ws!(i+1)) 0)
    joins = V.zipWith3 (buildJoin ss) normal1s normal2s ints
    joinSts = V.map lPathStart joins
    eds = V.snoc joinSts end -- the start of the join is the end of the prior segment
    joinSegs = V.map lPathSegs joins
    segs = V.snoc joinSegs [] -- just let the last join be empty, since it won't affect anything that way
    joinEnds = V.map lPathEnd joins
    sts = V.cons st joinEnds
    --joinEndsRot = V.imap (\i _ -> joinEnds!((i-1)`mod`n)) joinEnds
    parSegs = V.zipWith3 (parallelSegment ss) ws sts eds
    allSegs = V.zipWith (++) parSegs segs
    strokecs = concat allSegs
  in 
    makePath strokecs end

-- strokes the path
-- | produces the 'Contour' bounding the stroke of the 'Path'
strokePath :: StrokeStyle -> Path -> Contour
strokePath ss p =
  let
    ttol = tangentTolerance ss
    (ps,_) = pathSegs p
    seg1 = getWholeSegmentP 0 p
    segn = getWholeSegmentP ((length ps)-1) p
    begCap = buildCap ss (negify $ startTangent ttol seg1) (evaluate seg1 0.0)
    --begCap = buildCap ss (negify $ pathTangent seg1 0.0) (evaluate seg1 0.0)
    endCap = buildCap ss (endTangent ttol segn) (evaluate segn 1.0)
    --endCap = buildCap ss (pathTangent segn 1.0) (evaluate segn 1.0)
    Path{pathSegs=(ps1,_)} = strokePathExterior ss (lPathEnd begCap) (lPathStart endCap) p
    Path{pathSegs=(ps2,_)} = strokePathExterior ss (lPathEnd endCap) (lPathStart begCap) $ reverseP p
  in
    Contour $ (V.concat) [ps1, V.fromList $ lPathSegs endCap, ps2, V.fromList $ lPathSegs begCap]
    {-
    ( p1
    , endCap
    , p2
    , endCap
    , Contour $ (V.concat) [ps1, V.fromList $ lPathSegs endCap, ps2, V.fromList $ lPathSegs begCap]
    )
    -}
