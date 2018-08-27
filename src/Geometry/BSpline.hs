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

computeBSpline :: Vector Double -> Int -> Vector Double -> Double -> Double
computeBSpline knots d cpts x = 
  case V.findIndex (> x) knots of
    Nothing -> 
      0
    Just kp1 ->
      let 
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

computeBSplineGen :: Vector Double -> Int -> ControlPoints a -> Double -> Maybe a
computeBSplineGen knots d cpts x = 
  case V.findIndex (> x) knots of
    Nothing -> 
      Nothing
    Just kp1 ->
      let 
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

computeRBSpline :: Vector Double -> Int -> WControlPoints a -> Double -> Maybe a
computeRBSpline knots d (scaler,convComb,wpts) x = 
  let
    wts = V.map snd wpts
    spts = V.map (uncurry (flip scaler)) wpts
    w = computeBSpline knots d wts x
    mevalPt = computeBSplineGen knots d (convComb,spts) x
  in
    fmap (scaler (1/w)) mevalPt


