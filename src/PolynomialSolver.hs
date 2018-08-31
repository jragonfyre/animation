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

mathSign :: Double -> Double
mathSign x | x < 0 = -1
           | otherwise = 1

cubeRoot :: Double -> Double
cubeRoot t = (mathSign t) * ((abs t) ** (1/3))


-- using trig/hyperbolic method from wiki (cubic function)
-- requires a /= 0
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

solveCubicTol :: Double -> CubPoly -> [Double]
solveCubicTol tol (a,b,c,d) = 
  if abs a < tol
  then 
    solveQuadraticTol tol (b,c,d)
  else
    solveCubic (a,b,c,d)

solveQuadraticTol :: Double -> QuadPoly -> [Double]
solveQuadraticTol tol (a,b,c) =
  if abs a < tol
  then
    solveLinearTol tol (b,c)
  else
    solveQuadratic (a,b,c)

solveLinearTol :: Double -> LinPoly -> [Double]
solveLinearTol tol (a,b) = 
  if abs a < tol
  then
    []
  else
    solveLinear (a,b)

solveLinear :: LinPoly -> [Double]
solveLinear (a,b) = [-b/a]

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


