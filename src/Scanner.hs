--
-- Scanner.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

-- the point of this module is to produce types and helper functions for scan line rasterization that 
-- solves curves to find out where they cross a fixed y height positively and negatively 
-- and uses this to determine whether a pixel/point is inside the region

module Scanner
  ( module Scanner
  ) where

import Geometry

import Control.Lens ((^.), each, (&), (%~))

import Data.Maybe (mapMaybe)


data Sign = Plus | Minus 
  deriving (Show, Read, Eq, Ord, Enum)

signOf :: Double -> Sign
signOf x | x >= 0 = Plus
         | otherwise = Minus

signValue :: Sign -> Double
signValue Plus = 1
signValue Minus = -1

solveWPSeg :: Double -> WholePathSegment -> Double -> [(Double,Double,Sign)]
solveWPSeg tol (WPathSeg seg) = solveSegment tol seg
solveWPSeg tol (WPathBez2 bez) = solveBezier2 tol bez
solveWPSeg tol (WPathBez3 bez) = solveBezier3 tol bez

-- the second double is the absolute value of the x coordinate of the normal to the curve at the point.
-- used for antialiasing
solveWPSegNTF :: Double -> Double -> WholePathSegment -> [(Double,(Sign,Double))]
solveWPSegNTF tol y seg = map (\(t,xv,s)->(xv,(s,abs ((pathNormal seg t)^.x)))) $ solveWPSeg tol seg y 

--solveCusp :: Double -> WholePathSegment -> [Double]
--solveCusp 

-- returns t-values of cusp(s) if there is one
--solveCusp2 :: Double -> Bezier2 -> [Double]
--solveCusp2 tol bez = 

-- maybe t, x, sign
solveSegment :: Double -> Segment -> Double -> [(Double, Double, Sign)]
solveSegment tol seg y = 
  let
    ((sx,sy),(ex,ey)) = seg^.segAsPair & each %~ (^.ptAsPair)
    d = ey-sy
    t = (y-sy)/d
    x = sx + (ex-sx)*t
  in
    if (abs d) < tol
    then
      []
    else
      if (-tol) <= t && t <= 1+tol
      then
        [(t,x,signOf d)]
      else
        []

solveBezier2 :: Double -> Bezier2 -> Double -> [(Double, Double, Sign)]
solveBezier2 tol bez y =
  let
    (b2,b1,b0) = yCoeffs2 bez
    -- TODO fix the tolerance stuff
    ts = 
      if abs b2 < tol
      then 
        if abs b1 < tol
        then 
          []
        else
          [(y-b0)/b1]
      else
        solveQuadratic (b2,b1,b0-y)
    (a2,a1,a0) = xCoeffs2 bez
    derivy t = 2*t*b2 + b1
    derivx t = 2*t*a2 + a1
    computex t = t*(t*a2+a1)+a0
    isValid t = 
        --Just (t,t*(t*a2+a1)+a0,signOf (derivy t))
      if -tol <= t && t <= 1+tol
      then 
        Just (t,computex t,signOf (derivy t))
      else
        Nothing
  in
    if (withinY (boundingBox2 bez) y)
    then 
      case ts of 
        [] ->
          []
        [t1,t2] ->
          if t1 == t2
          then
            let 
              t = t1
              ta=t1-2*tol
              tb=t1+2*tol
              dx = derivx t1
              x = computex t
              epsilon = tol * dx/(abs dx)
              xa = computex ta
              xb = computex tb
            in
              mapMaybe isValid [ta,tb]
              {-
              if -tol <= t && t <= 1+tol
              then
                case () of 
                  _ | b2 < 0  -> [(t,x-epsilon,Plus),(t,x+epsilon,Minus)]
                    | b2 > 0  -> [(t,x-epsilon,Minus),(t,x+epsilon,Plus)]
                    | b2 == 0 -> []
              else
                []
              -}
          else
            mapMaybe isValid ts 
            --zip3 ts (map computex ts) (map (signOf . derivy) ts)
        _ -> 
          mapMaybe isValid ts
    else
      []

solveBezier3 :: Double -> Bezier3 -> Double -> [(Double,Double,Sign)]
solveBezier3 tol bez y =
  let
    (b3,b2,b1,b0) = yCoeffs3 bez
    (a3,a2,a1,a0) = xCoeffs3 bez
    ts = 
      if abs b3 < tol
      then
        if abs b2 < tol
        then
          if abs b1 < tol
          then
            []
          else
            [(y-b0)/b1]
        else
          solveQuadratic (b2,b1,b0-y)
      else
        solveCubic (b3,b2,b1,b0-y)
    derivy t = t*(3*t*b3+2*b2)+b1
    paramx t = t*(t*(t*a3+a2)+a1)+a0
    isValid t = 
      if -tol <= t && t <= 1+tol
      then 
        Just (t,paramx t,signOf (derivy t))
      else
        Nothing
  in
    if (withinY (boundingBox3 bez) y)
    then 
      mapMaybe isValid ts
    else
      []


mathSign :: Double -> Double
mathSign x | x < 0 = -1
           | otherwise = 1

cubeRoot :: Double -> Double
cubeRoot t = (mathSign t) * ((abs t) ** (1/3))

-- using trig/hyperbolic method from wiki (cubic function)
solveCubic :: (Double, Double, Double, Double) -> [Double]
solveCubic (a,b,c,d) = 
  let
    diff = -b/(3*a)
    bn = b/a
    cn = c/a
    dn = d/a
    bn2 = bn^2
    p = cn - bn2/3
    q = (2/27)*bn*bn2 - (bn*cn/3) + dn
  in
    map (+diff) $ solveDepressedCubic (p,q)

solveDepressedCubic :: (Double, Double) -> [Double]--([Double],[Double])
solveDepressedCubic (p,q) = 
  let
    delta = -4*p^3 - 27*q^2
    p3 = sqrt (-p/3)
    pp3 = sqrt (p/3)
    signq = q/(abs q)
    const1 = 3*q/(2*p)
    acosInnards = const1/p3
    acoshInnards = -acosInnards*signq
    asinhInnards = const1/pp3
    cosFormula k = 2*p3 * (cos (((acos acosInnards) - 2*pi*(fromIntegral k))/3))
    coshFormula = -2 * signq * p3 * (cosh ((acosh acoshInnards)/3))
    sinhFormula = -2 * pp3 * (sinh ((asinh asinhInnards)/3))
  in
    if abs p <= 10 ** (-10)
    then
      (replicate 3 (cubeRoot (-q)))
      --([p],replicate 3 (cubeRoot (-q)))
    else
      if delta >= 0
      then
        (map cosFormula [0,1,2])
        --([delta,p3,acosInnards],map cosFormula [0,1,2])
      else
        if p < 0
        then
          ([coshFormula])
          --([delta,p3,acoshInnards],[coshFormula])
        else
          ([sinhFormula])
          --([delta,pp3,asinhInnards],[sinhFormula])

solveQuadratic :: (Double, Double, Double) -> [Double]
solveQuadratic (b2,b1,b0) = 
  let
    (bo2,c) = (b1/(2*b2),b0/b2)
    d = bo2^2 - c
    delta = sqrt d
    ts = if d < 0 then [] else [-bo2-delta,-bo2+delta]
  in
    ts

