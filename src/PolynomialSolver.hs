--
-- PolynomialSolver.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module PolynomialSolver
  ( module PolynomialSolver
  ) where

import Polynomial

-- | Takes a tolerance @tol@ 
--   to determine whether the leading coefficient is nonzero, and thus make a decision about
--   whether to call 'solveCubic' or recursively call @'solveQuadraticTol' tol@.
solveCubicTol :: Double -> CubPoly -> [Double]
solveCubicTol tol (a,b,c,d) = 
  if abs a < tol
  then 
    solveQuadraticTol tol (b,c,d)
  else
    solveCubic (a,b,c,d)

-- | Takes a tolerance @tol@ 
--   to determine whether the leading coefficient is nonzero, and thus make a decision about
--   whether to call 'solveQuadratic' or recursively call @'solveLinearTol' tol@.
solveQuadraticTol :: Double -> QuadPoly -> [Double]
solveQuadraticTol tol (a,b,c) =
  if abs a < tol
  then
    solveLinearTol tol (b,c)
  else
    solveQuadratic (a,b,c)

-- | Takes a tolerance @tol@ 
--   to determine whether the leading coefficient is nonzero, and thus make a decision about
--   whether to call 'solveLinear' or determine that there are no solutions (even in the case where the
--   equation reduces to @0 == 0@, since there's currently no good way to describe the case that all numbers 
--   are solutions, as well as presently no reason to care about this case.
solveLinearTol :: Double -> LinPoly -> [Double]
solveLinearTol tol (a,b) = 
  if abs a < tol
  then
    []
  else
    solveLinear (a,b)

-- using trig/hyperbolic method from wiki (cubic function)
-- requires a /= 0
-- | Finds the roots of a 'CubPoly' using the methods from 
--   <https://en.wikipedia.org/wiki/Cubic_function#Trigonometric_and_hyperbolic_solutions Wikipedia>.
--
--   __Requires:__ The leading coefficient of the polynomial must be nonzero.
solveCubic :: CubPoly -> [Double]
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

-- | Finds the roots of a 'QuadPoly'.
--
--   __Requires:__ The leading coefficient of the polynomial must be nonzero.
solveQuadratic :: QuadPoly -> [Double]
solveQuadratic (b2,b1,b0) = 
  let
    (bo2,c) = (b1/(2*b2),b0/b2)
    d = bo2^2 - c
    delta = sqrt d
    ts = if d < 0 then [] else [-bo2-delta,-bo2+delta]
  in
    ts

-- | Finds the roots of a @LinPoly@. 
--
--   __Requires:__ The leading coefficient of the polynomial must be nonzero.
solveLinear :: LinPoly -> [Double]
solveLinear (a,b) = [-b/a]

-- | Finds the roots of a depressed cubic polynomial, called by 'solveCubic'.
--   Probably no reason to call this directly.
solveDepressedCubic :: (Double, Double) -- ^ @(p,q)@ are the coefficients of the polynomial 
                                        --   @t^3 + p*t + q@.
                    -> [Double]--([Double],[Double])
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



-- | Helper function. I'll probably remove this from the export list.
mathSign :: Double -> Double
mathSign x | x < 0 = -1
           | otherwise = 1

-- | Helper function. I'll probably remove this from the export list.
cubeRoot :: Double -> Double
cubeRoot t = (mathSign t) * ((abs t) ** (1/3))

