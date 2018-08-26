--
-- Parametrized.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Parametrized
  ( module Geometry.Parametrized
  ) where

import Geometry.Types
import Geometry.Affine
import Geometry.Path

import Control.Lens
import Control.Lens.TH

import qualified Data.Vector as V


{-
class Differentiable a b | a -> b where
instance Differentiable Point Vector where
instance Differentiable Vector Vector where
instance Differentiable Double Double where

type Function0 a = a -> a

data Function1 a b c = Function1
  { _function1Func :: a -> b
  , _function1Deriv :: a -> (c -> c)
  }

makeFields ''Function1

composeF1 :: Function1 a b c d -> Function1 c d e f
-}

data ParamPath0 = ParamPath0
  { _paramPath0Func :: Double -> Point
  , _paramPath0Range :: (Double,Double)
  }
data ParamPath1 = ParamPath1
  { _paramPath1Param :: ParamPath0
  , _paramPath1Deriv :: Double -> Vector
  }

makeFields ''ParamPath0
makeFields ''ParamPath1

makeGraph :: (Double,Double) -> (Double -> Double) -> ParamPath0
makeGraph rg f = 
  ParamPath0 (\t -> makePoint t (f t)) rg

isNormalizedPP0 :: ParamPath0 -> Bool
isNormalizedPP0 = (==(0,1)) . (^.range)

normalizePP0 :: ParamPath0 -> ParamPath0
normalizePP0 pp =
  let
    (st,ed) = pp^.range
  in 
    ParamPath0 ((pp^.func) . (\t -> (st-ed)*t+st)) (0,1)

isNormalizedPP1 :: ParamPath1 -> Bool
isNormalizedPP1 = isNormalizedPP0 . (^.param)

normalizePP1 :: ParamPath1 -> ParamPath1
normalizePP1 pp1 =
  let
    pp=pp1^.param
    (st,ed) = pp^.range
    adjustRange t = (st-ed)*t+st
  in 
    ParamPath1
      (ParamPath0 ((pp^.func) . adjustRange) (0,1))
      (((st-ed)*.) . (pp1^.deriv) . adjustRange)

data ParamContour0 = ParamContour0
  { _paramContour0Func :: Double -> Point
  , _paramContour0Period :: Double
  }
data ParamContour1 = ParamContour1
  { _paramContour1Param :: ParamContour0
  , _paramContour1Deriv :: Double -> Vector
  }

makeFields ''ParamContour0
makeFields ''ParamContour1

isNormalizedPC0 :: ParamContour0 -> Bool
isNormalizedPC0 = (==1) . (^.period)

normalizePC0 :: ParamContour0 -> ParamContour0
normalizePC0 pc =
  let
    prd = pc^.period
  in 
    ParamContour0 ((pc^.func) . (\t -> prd*t)) 1

isNormalizedPC1 :: ParamContour1 -> Bool
isNormalizedPC1 = isNormalizedPC0 . (^.param)

normalizePC1 :: ParamContour1 -> ParamContour1
normalizePC1 pc1 =
  let
    pc=pc1^.param
    prd = pc^.period
    adjustRange t = prd*t
  in 
    ParamContour1
      (ParamContour0 ((pc^.func) . adjustRange) 1)
      ((prd*.) . (pc1^.deriv) . adjustRange)


buildParametrizedPath :: ParamPath0 -> Double -> Path
buildParametrizedPath pp pstep =
  let
    p = pp^.func
    (pstart,pend) = pp^.range
    n = ceiling $ (pend-pstart)/pstep
    ts = fmap ((pstart+) . (pstep *) . fromIntegral) [0..(n-1)]
    lt = pstart + pstep*(fromIntegral n)
  in
    makePath (fmap (PathSeg . p) ts) (p lt)

buildParametrizedContour :: ParamContour0 -> Double -> Contour
buildParametrizedContour pp pstep =
  let
    p = pp^.func
    pperiod = pp^.period
    n = ceiling $ pperiod/pstep
    ts = fmap ((pstep *) . fromIntegral) [0..(n-1)]
  in
    makeContour (fmap (PathSeg . p) ts)


buildBez2ApproxPC1 :: Double -> ParamContour1 -> Double -> Contour
buildBez2ApproxPC1 tol pp1 pstep = 
  let
    pp = pp1^.param
    pd = pp1^.deriv
    p = pp^.func
    prd = pp^.period
    n = ceiling $ prd/pstep
    ts = fmap ((pstep *) . fromIntegral) [0..(n-1)]
    sts = V.fromList $ fmap p ts
    eds = V.imap (\i _ -> (V.!) sts ((i+1)`mod` n)) sts
    sdrvs = V.fromList $ fmap pd ts
    edrvs = V.imap (\i _ -> (V.!) sdrvs ((i+1)`mod`n)) sdrvs
    -- st + t sdrv = ed + s edrv -- want to solve
    mats = V.zipWith (makeMatrix) sdrvs edrvs
    dets = V.map det mats
    -- uhoh what if there's no inversion xD lol, e.g. at a cusp or something, or when things are parallel
    -- if things are parallel, we could just use a line in the path. look into it later
    -- TODO
    imats = V.map invertMatrix mats
    diffs = V.zipWith (-.) sts eds
    solns = V.zipWith (*.) imats diffs
    tvals = V.map (negate . (^.x)) solns
    cpts = V.zipWith (+.) sts (V.zipWith (*.) tvals sdrvs)
    bezs = V.zipWith PathBez2 sts cpts
    lins = V.map PathSeg sts
  in
    Contour $ V.zipWith3 (\d b l -> if abs d < tol then l else b) dets bezs lins

{-
--too weak. The approximation doesn't properly work
buildBez3ApproxPC1 :: ParamContour1 -> Double -> Contour
buildBez3ApproxPC1 pp1 pstep = 
  let
    pp = pp1^.param
    pd = pp1^.deriv
    p = pp^.func
    prd = pp^.period
    n = ceiling $ prd/pstep
    ts = fmap ((pstep *) . fromIntegral) [0..(n-1)]
    sts = V.fromList $ fmap p ts
    eds = V.imap (\i _ -> (V.!) sts ((i+1)`mod` n)) sts
    sdrvs = V.fromList $ fmap pd ts
    edrvs = V.imap (\i _ -> (V.!) sdrvs ((i+1)`mod`n)) sdrvs
    scpts = V.zipWith (+.) sts (V.map ((1/3::Double)*.) sdrvs)
    ecpts = V.zipWith (-.) eds (V.map ((1/3::Double)*.) edrvs)
    bezs = V.zipWith3 PathBez3 sts scpts ecpts
  in
    Contour bezs
-}

--buildBez2ApproxPP1 :: ParamPath1 -> Double -> Path




