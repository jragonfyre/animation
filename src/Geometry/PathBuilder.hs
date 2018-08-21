--
-- PathBuilder.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.PathBuilder
  ( module Geometry.PathBuilder
  ) where

import Geometry.Types
import Geometry.Path
import Geometry.Affine

import Control.Lens ((^.))

-- based on SVG path commands
data PathCommand 
  = MA !Double !Double 
--  | M !Double !Double
  | LA !Double !Double
  | L !Double !Double
  | HA !Double
  | H !Double
  | VA !Double
  | V !Double
  | CA !Double !Double !Double !Double !Double !Double
  | C !Double !Double !Double !Double !Double !Double
  | SA !Double !Double !Double !Double
  | S !Double !Double !Double !Double
  | QA !Double !Double !Double !Double
  | Q !Double !Double !Double !Double
  | TA !Double !Double
  | T !Double !Double
  | Z
  deriving (Read, Show, Eq, Ord)


-- if you do stupid stuff, this won't work. Assumes that every path starts with a move absolute command
-- also every path ends with a Z command
-- works with the outline of Q below
process :: [PathCommand] -> ClosedPath
process [] = []
process ((MA x y):cs) = 
  let
    pt = makePoint x y
  in 
    processHelper Pt pt pt [] cs

data SegType = Pt | LSeg | QBez | CBez

processHelper :: SegType -> Point -> Point -> [PathSegment] -> [PathCommand] -> ClosedPath
processHelper _ le _ segs ((LA x y):cs) =
  let
    pt = makePoint x y
  in 
    processHelper LSeg pt pt ((PathSeg le):segs) cs
processHelper _ le _ segs ((L x y):cs) =
  let
    vec = makeVector x y
    pt = le +. vec
  in 
    processHelper LSeg pt pt ((PathSeg le):segs) cs
processHelper _ le _ segs ((HA x):cs) =
  let
    pt = makePoint x (le^.y)
  in 
    processHelper LSeg pt pt ((PathSeg le):segs) cs
processHelper _ le _ segs ((H x):cs) =
  let
    vec = makeVector x 0
    pt = le +. vec
  in 
    processHelper LSeg pt pt ((PathSeg le):segs) cs
processHelper _ le _ segs ((VA y):cs) =
  let
    pt = makePoint (le^.x) y
  in 
    processHelper LSeg pt pt ((PathSeg le):segs) cs
processHelper _ le _ segs ((V y):cs) =
  let
    vec = makeVector 0 y
    pt = le +. vec
  in 
    processHelper LSeg pt pt ((PathSeg le):segs) cs
processHelper _ s _ segs ((CA cx cy dx dy ex ey):cs) =
  let
    c = makePoint cx cy
    d = makePoint dx dy
    e = makePoint ex ey
  in 
    processHelper CBez e d ((PathBez3 s c d):segs) cs
processHelper _ s _ segs ((C cx cy dx dy ex ey):cs) =
  let
    cv = makeVector cx cy
    dv = makeVector dx dy
    ev = makeVector ex ey
    c = s+.cv
    d = s+.dv
    e = s+.ev
  in 
    processHelper CBez e d ((PathBez3 s c d):segs) cs
processHelper CBez s ld segs ((SA dx dy ex ey):cs) =
  let
    c = s +. (s -. ld)
    d = makePoint dx dy
    e = makePoint ex ey
  in 
    processHelper CBez e d ((PathBez3 s c d):segs) cs
processHelper CBez s ld segs ((S dx dy ex ey):cs) =
  let
    dv = makeVector dx dy
    ev = makeVector ex ey
    c = s +. (s -. ld)
    d = s+.dv
    e = s+.ev
  in 
    processHelper CBez e d ((PathBez3 s c d):segs) cs
processHelper _ s _ segs ((SA dx dy ex ey):cs) =
  let
    c = s
    d = makePoint dx dy
    e = makePoint ex ey
  in 
    processHelper CBez e d ((PathBez3 s c d):segs) cs
processHelper _ s _ segs ((S dx dy ex ey):cs) =
  let
    dv = makeVector dx dy
    ev = makeVector ex ey
    c = s
    d = s+.dv
    e = s+.ev
  in 
    processHelper CBez e d ((PathBez3 s c d):segs) cs
processHelper _ s _ segs ((QA cx cy ex ey):cs) =
  let
    c = makePoint cx cy
    e = makePoint ex ey
  in 
    processHelper QBez e c ((PathBez2 s c):segs) cs
processHelper _ s _ segs ((Q cx cy ex ey):cs) =
  let
    cv = makeVector cx cy
    ev = makeVector ex ey
    c = s+.cv
    e = s+.ev
  in 
    processHelper QBez e c ((PathBez2 s c):segs) cs
processHelper QBez s ld segs ((TA ex ey):cs) =
  let
    c = s +. (s -. ld)
    e = makePoint ex ey
  in 
    processHelper QBez e c ((PathBez2 s c):segs) cs
processHelper QBez s ld segs ((T ex ey):cs) =
  let
    ev = makeVector ex ey
    c = s +. (s -. ld)
    e = s+.ev
  in 
    processHelper QBez e c ((PathBez2 s c):segs) cs
processHelper _ s _ segs ((TA ex ey):cs) =
  let
    c = s
    e = makePoint ex ey
  in 
    processHelper QBez e c ((PathBez2 s c):segs) cs
processHelper _ s _ segs ((T ex ey):cs) =
  let
    ev = makeVector ex ey
    c = s
    e = s+.ev
  in 
    processHelper QBez e c ((PathBez2 s c):segs) cs
processHelper _ _ _ segs (Z:cs) = (makeContour (reverse segs)):(process cs)

-- extracted Caudex Regular glyph for Q in SVG-font format
-- <glyph glyph-name="Q" unicode="Q" horiz-adv-x="1610" 
-- d="M808 1446h10q323 0 545 -198q207 -185 207 -488q0 -27 -2 -55q-21 -344 -238 -533q-209 -182 -499 -205q81 -18 200 -71q179 -80 273 -80q34 0 100 19v-50q-97 -56 -142 -105q-123 16 -321.5 97.5t-265.5 81.5q-38 0 -150 -74v63q106 82 145 105q14 8 29 13
-- q-241 19 -431 190q-220 196 -220 513q0 18 1 36q12 341 220.5 539t538.5 202zM808 1358q-307 -1 -429 -193.5t-122 -462.5q0 -271 153 -462.5t398 -193.5h5q239 0 393 204q152 202 152 446v6q-2 249 -130 452.5t-420 203.5z" /> 

-- boundary of the Q

glyphQ :: ClosedPath
glyphQ = process wholeQ

wholeQ :: [PathCommand]
wholeQ = qbdry ++ qint

qbdry :: [PathCommand]
qbdry = 
  [ MA 808 1446
  , H 10
  , Q 323 0 545 (-198)
  , Q 207 (-185) 207 (-488)
  , Q 0 (-27) (-2) (-55)
  , Q (-21) (-344) (-238) (-533)
  , Q (-209) (-182) (-499) (-205)
  , Q 81 (-18) 200 (-71)
  , Q 179 (-80) 273 (-80)
  , Q 34 0 100 19
  , V (-50)
  , Q (-97) (-56) (-142) (-105)
  , Q (-123) 16 (-321.5) 97.5
  , T (-265.5) 81.5
  , Q (-38) 0 (-150) (-74)
  , V 63
  , Q 106 82 145 105
  , Q 14 8 29 13
  , Q (-241) 19 (-431) 190
  , Q (-220) 196 (-220) 513
  , Q 0 18 1 36
  , Q 12 341 220.5 539
  , T 538.5 202
  , Z
  ]

qint :: [PathCommand]
-- interior of the Q
qint = 
  [ MA 808 1358
  , Q (-307) (-1) (-429) (-193.5)
  , T (-122) (-462.5)
  , Q 0 (-271) 153 (-462.5)
  , T 398 (-193.5)
  , H 5
  , Q 239 0 393 204
  , Q 152 202 152 446
  , V 6
  , Q (-2) 249 (-130) 452.5
  , T (-420) 203.5
  , Z
  ]

