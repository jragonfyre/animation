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
import PolynomialSolver

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
solveWPSeg tol (WPathEArc earc) = solveEArc tol earc

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

solveEArc :: Double -> EllipticalArc -> Double -> [(Double,Double,Sign)]
solveEArc tol earc yv = 
  let
    ell = earc^.ellipse
    cent = ell^.center
    (cx,cy) = cent^.ptAsPair
    (mat,_) = ell^.matrix
    ((a,c),(b,d))=mat^.matAsComponents
    rely = yv-cy
    possXs = solveQuadratic (a,(b+c)*rely,d*rely^2-1)
    rx = ell^.radX
    ry = ell^.radY
    ph = ell^.phi
    st = earc^.start
    delt = earc^.delta
    rot = rotate (-ph)
    dtol = abs (tol*delt)
    isValid relx = 
      let
        v = makeVector relx rely
        (rxcth, rysth) = (rot *. v)^.vecAsPair
        thet1 = angle xVec (makeVector (rxcth/rx) (rysth/ry))
        thets = filter (\thc -> if delt >= 0
                                then (st - dtol) <= thc && thc <= (st + delt + dtol)
                                else (st + delt - dtol) <= thc && thc <= (st + dtol)
                       )
                       [thet1 - 2*pi, thet1, thet1 + 2*pi]
        ts = map (\thet -> (thet-st)/delt) thets
      in 
        case ts of 
          [] -> Nothing
          [t] -> Just (t,relx+cx,signOf ((derivativeEArc earc t)^.y))
          -- _ -> Nothing
  in
    mapMaybe isValid possXs
    --if (withinY (bounds earc) y)

