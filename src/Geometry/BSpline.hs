--
-- BSpline.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.BSpline
  ( module Geometry.BSpline
  ) where

-- need to implement de Boor's algorithm
-- rational bsplines can be split into rational beziers and similarly
-- bsplines can be split into beziers, so we should be able to plot these simply?

import Data.Vector (Vector,MVector,(!))
import qualified Data.Vector as V
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as M

import Control.Monad.ST
import Data.STRef

import Geometry.Affine
import Geometry.Types (x,y,makePoint,Point)
import Geometry.Bezier
import Geometry.Path

import Control.Lens ((^.))

import Polynomial

test :: ST s Double
test = do
  ref <- newSTRef 1
  readSTRef ref

val :: Double
val = runST test

-- the function is the convex combination function
-- cnvComb p1 p2 0 = p1
-- cnvComb p1 p2 1 = p2

type ConvexCombinator a = a -> a -> Double -> a
-- yes, this is a pun. it is not supposed to be scalar... jk Scaler was a bad name
type Scaling a = Double -> a -> a

type ControlPoints a = (ConvexCombinator a, Vector a)

type WControlPoints a = (Scaling a, ConvexCombinator a, Vector (a,Double))

convCombOfVecs :: (Summable a a a, Multiplicable Double a a) => a -> a -> Double -> a
convCombOfVecs p1 p2 t = (1-t)*.p1 +. t*.p2


makeControlPoints :: (Summable a a a, Multiplicable Double a a) => Vector a -> ControlPoints a
makeControlPoints = (convCombOfVecs,)

makeWControlPoints :: (Summable a a a, Multiplicable Double a a) => Vector (a,Double) -> WControlPoints a
makeWControlPoints = ((*.),convCombOfVecs,)

-- seems like I have to put this out here, in order to put this type signature on it??? Wtf is going on even?
cbsMainLoop :: Vector Double -> Int -> Vector Double -> Double -> Int -> Int -> Double
cbsMainLoop knots d cpts x k p = cbsMainLoopGen knots d (\d1 d2 t -> (1-t)*d1+t*d2,cpts) x k p

cbsMainLoopGen :: Vector Double -> Int -> ControlPoints a -> Double -> Int -> Int -> a
cbsMainLoopGen knots d (cnvComb,cpts) x k p = runST $ do
  ds <- M.new d
  sequence_ $
    map (\j -> M.write ds j (cpts!(j+k-p))) [0..p]
  sequence_ $
    map 
      (\r -> 
        sequence_ $
          map 
            (\j -> 
              do
                let alphaj = (x-(knots!(j+k-p)))/((knots!(j+1+k-r))-(knots!(j+k-p)))
                djm1 <- M.read ds (j-1)
                dj <- M.read ds j
                M.write ds j $ cnvComb djm1 dj alphaj -- (1-alphaj)*djm1 + alphaj*dj
            ) 
            [p,p-1..r]
      )
      [1..p]
  M.read ds p

--debug

computeBSpline :: Vector Double -> Vector Double -> Double -> Double
computeBSpline knots cpts x = 
  case V.findIndex (> x) knots of
    Nothing -> 
      let
        --m = V.length knots
        n = V.length cpts
        --d = m-n
        --p = d-1
        -- index of last internal knot
        ixlik = n -- n == (m-1-p) == (m-d)
      in
        if x <= (knots!ixlik)
        then
          cpts!(n-1)
        else
          0
    Just kp1 ->
      let 
        d = V.length knots - V.length cpts
        k = kp1-1
        p = d-1
      in
        if k-p >= 0 && kp1+p < (length knots)
        then
          -- de Boor's algorithm, straight from Wiki
          -- implemented with MVectors and Control.Monad.ST
          cbsMainLoop knots d cpts x k p
        else
          0

computeBSplineGen :: Vector Double -> ControlPoints a -> Double -> Maybe a
computeBSplineGen knots cpts@(_,pts) x = 
  case V.findIndex (> x) knots of
    Nothing -> 
      let
        --m = V.length knots
        n = V.length pts
        --d = m-n
        --p = d-1
        -- index of last internal knot
        ixlik = n -- n == (m-1-p) == (m-d)
      in
        if x <= (knots!ixlik)
        then
          Just (pts!(n-1))
        else
          Nothing
    Just kp1 ->
      let 
        -- d is the order of the spline, p is the degree
        d = V.length knots - V.length pts
        k = kp1-1
        p = d-1
      in
        if k-p >= 0 && kp1+p < (length knots)
        then
          -- de Boor's algorithm, straight from Wiki
          -- implemented with MVectors and Control.Monad.ST
          Just $ cbsMainLoopGen knots d cpts x k p
        else
          Nothing

computeRBSpline :: Vector Double -> WControlPoints a -> Double -> Maybe a
computeRBSpline knots (scaler,convComb,wpts) x = 
  let
    wts = V.map snd wpts
    spts = V.map (uncurry (flip scaler)) wpts
    w = computeBSpline knots wts x
    mevalPt = computeBSplineGen knots (convComb,spts) x
  in
    fmap (scaler (1/w)) mevalPt

-- this number is the degree of the curve, not its order
-- also this is almost certainly not the most efficient way to go about this, it's just the easiest :)
-- also ignores the provided functions, since it expects the usual definition
-- pretty sure this doesn't work yet, think there's a bug in the handling of rational BSplines, but for now,
-- as long as it handles the nonrational case, that's still progress
-- think i have to premultiply the points by the weights before computing the new xs then divide again...
-- :/
-- think I fixed this in rBSplineToPath2
-- YEP! :D
{-
rBSplineToBeziers2 :: Vector Double -> Vector WPoint -> [RBezier2]
rBSplineToBeziers2 knots wpts = 
  let
    m = length knots
    n = length wpts -- hence there are n basis functions, and at most n nontrivial knot intervals on which 
    -- the correct number of basis functions (3) are defined
    d = m - n -- this had *better* be 3
    p = d-1 -- this should therefore be 2
    iKnots = V.slice p (m-2*p) knots
    --uniqks = V.uniq knots
    kints = filter (uncurry (/=)) . V.toList . V.zip iKnots $ V.tail iKnots -- the knot intervals
    pts = V.map fst wpts
    xs = V.map (^.x) pts
    ys = V.map (^.y) pts
    ws = V.map snd wpts
    computeX = computeBSpline knots xs
    computeY = computeBSpline knots ys
    computeW = computeBSpline knots ws
    kiToRBez2 (s,e) = 
      let
        (sx,cx,ex) = standardToBezierBasis2 $ interpolate2Std (computeX s) (computeX ((s+e)/2)) (computeX e)
        (sy,cy,ey) = standardToBezierBasis2 $ interpolate2Std (computeY s) (computeY ((s+e)/2)) (computeY e)
        (sw,cw,ew) = standardToBezierBasis2 $ interpolate2Std (computeW s) (computeW ((s+e)/2)) (computeW e)
      in
        makeRBezier2 (makePoint sx sy,sw) (makePoint cx cy,cw) (makePoint ex ey,ew)
  in
    map kiToRBez2 kints
-}

rBSplineToPath2 :: Vector Double -> Vector WPoint -> Path
rBSplineToPath2 knots wpts = 
  let
    m = length knots
    n = length wpts -- hence there are n basis functions, and at most n nontrivial knot intervals on which 
    -- the correct number of basis functions (3) are defined yeah idk abt this, prolly true tho???
    d = m - n -- this had *better* be 3
    p = d-1 -- this should therefore be 2
    numiKnots = m-2*p
    numkints = numiKnots-1
    iKnots = V.slice p numiKnots knots
    --uniqks = V.uniq knots
    kints = V.filter (uncurry (/=)) . V.zip iKnots $ V.tail iKnots -- the knot intervals
    kmids = V.map (\(x,y) -> (x+y)/2) kints
    pts = V.map fst wpts
    ws = V.map snd wpts
    xhs = V.zipWith (*) ws $ V.map (^.x) pts
    yhs = V.zipWith (*) ws $ V.map (^.y) pts
    computeXh = computeBSpline knots xhs
    computeYh = computeBSpline knots yhs
    computeW = computeBSpline knots ws
    exhs = V.map computeXh iKnots
    eyhs = V.map computeYh iKnots
    ews = V.map computeW iKnots
    mxhs = V.map computeXh kmids
    myhs = V.map computeYh kmids
    mws = V.map computeW kmids
    segs = V.generate numkints $ \i -> 
      let 
        (sxh,cxh,_) = stIntToBezBasis2 (exhs!i, mxhs!i, exhs!(i+1))
        --(sxh,cxh,_) = standardToBezierBasis2 $ interpolate2Std (exhs!i) (mxhs!i) (exhs!(i+1))
        (syh,cyh,_) = stIntToBezBasis2 (eyhs!i, myhs!i, eyhs!(i+1))
        --(syh,cyh,_) = standardToBezierBasis2 $ interpolate2Std (eyhs!i) (myhs!i) (eyhs!(i+1))
        (sw,cw,ew) = stIntToBezBasis2 (ews!i, mws!i, ews!(i+1))
        --(sw,cw,ew) = standardToBezierBasis2 $ interpolate2Std (ews!i) (mws!i) (ews!(i+1))
      in
        makeRBez2Seg (makePoint (sxh/sw) (syh/sw),sw) (makePoint (cxh/cw) (cyh/cw),cw) ew
    cap = makePoint ((exhs!numkints)/(ews!numkints)) ((eyhs!numkints)/(ews!numkints))
  in
    Path (segs,cap)

-- YAY THIS WORKS TOOOOO :D
rBSplineToPath3 :: Vector Double -> Vector WPoint -> Path
rBSplineToPath3 knots wpts = 
  let
    m = length knots
    n = length wpts -- hence there are n basis functions, and at most n nontrivial knot intervals on which 
    -- the correct number of basis functions (4) are defined
    d = m - n -- this had *better* be 4
    p = d-1 -- this should therefore be 3
    numiKnots = m-2*p
    numkints = numiKnots-1
    iKnots = V.slice p numiKnots knots
    --uniqks = V.uniq knots
    kints = V.filter (uncurry (/=)) . V.zip iKnots $ V.tail iKnots -- the knot intervals
    kmids1 = V.map (\(x,y) -> (2*x+y)/3) kints
    kmids2 = V.map (\(x,y) -> (x+2*y)/3) kints
    pts = V.map fst wpts
    ws = V.map snd wpts
    xhs = V.zipWith (*) ws $ V.map (^.x) pts
    yhs = V.zipWith (*) ws $ V.map (^.y) pts
    computeXh = computeBSpline knots xhs
    computeYh = computeBSpline knots yhs
    computeW = computeBSpline knots ws
    exhs = V.map computeXh iKnots
    eyhs = V.map computeYh iKnots
    ews = V.map computeW iKnots
    m1xhs = V.map computeXh kmids1
    m1yhs = V.map computeYh kmids1
    m1ws = V.map computeW kmids1
    m2xhs = V.map computeXh kmids2
    m2yhs = V.map computeYh kmids2
    m2ws = V.map computeW kmids2
    segs = V.generate numkints $ \i -> 
      let 
        (sxh,cxh,dxh,_) = stIntToBezBasis3 (exhs!i, m1xhs!i, m2xhs!i, exhs!(i+1))
        (syh,cyh,dyh,_) = stIntToBezBasis3 (eyhs!i, m1yhs!i, m2yhs!i, eyhs!(i+1))
        (sw,cw,dw,ew) = stIntToBezBasis3 (ews!i, m1ws!i, m2ws!i, ews!(i+1))
        --(sxh,cxh,dxh,_) = standardToBezierBasis3 $ interpolate3Std (exhs!i) (m1xhs!i) (m2xhs!i) (exhs!(i+1))
        --(syh,cyh,dyh,_) = standardToBezierBasis3 $ interpolate3Std (eyhs!i) (m1yhs!i) (m2yhs!i) (eyhs!(i+1))
        --(sw,cw,dw,ew) = standardToBezierBasis3 $ interpolate3Std (ews!i) (m1ws!i) (m2ws!i) (ews!(i+1))
      in
        makeRBez3Seg 
          (makePoint (sxh/sw) (syh/sw),sw)
          (makePoint (cxh/cw) (cyh/cw),cw)
          (makePoint (dxh/dw) (dyh/dw),dw)
          ew
    cap = makePoint ((exhs!numkints)/(ews!numkints)) ((eyhs!numkints)/(ews!numkints))
  in
    Path (segs,cap)

