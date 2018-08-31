--
-- Polynomial.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Polynomial
  ( module Polynomial
  ) where

import MathClasses

type CPoly = Double
type LinPoly = (Double,Double)
type QuadPoly = (Double,Double,Double)
type CubPoly = (Double,Double,Double,Double)

instance Summable LinPoly LinPoly LinPoly where
  (+.) (a1,a0) (b1,b0) = (a1+b1,a0+b0)
instance Subtractable LinPoly LinPoly LinPoly where
  (-.) (a1,a0) (b1,b0) = (a1-b1,a0-b0)
instance Multiplicable Double LinPoly LinPoly where
  (*.) x (a1,a0) = (x*a1,x*a0)
instance Negatable LinPoly where
  negify (a1,a0) = (-a1,-a0)
instance Zeroable LinPoly where
  zero = (0,0)
instance Unitable LinPoly where
  unit = (0,1)
instance Summable QuadPoly QuadPoly QuadPoly where
  (+.) (a2,a1,a0) (b2,b1,b0) = (a2+b2,a1+b1,a0+b0)
instance Subtractable QuadPoly QuadPoly QuadPoly where
  (-.) (a2,a1,a0) (b2,b1,b0) = (a2-b2,a1-b1,a0-b0)
instance Multiplicable Double QuadPoly QuadPoly where
  (*.) x (a2,a1,a0) = (x*a2,x*a1,x*a0)
instance Negatable QuadPoly where
  negify (a2,a1,a0) = (-a2,-a1,-a0)
instance Zeroable QuadPoly where
  zero = (0,0,0)
instance Unitable QuadPoly where
  unit = (0,0,1)
instance Summable CubPoly CubPoly CubPoly where
  (+.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3+b3,a2+b2,a1+b1,a0+b0)
instance Subtractable CubPoly CubPoly CubPoly where
  (-.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3-b3,a2-b2,a1-b1,a0-b0)
instance Multiplicable Double CubPoly CubPoly where
  (*.) x (a3,a2,a1,a0) = (x*a3,x*a2,x*a1,x*a0)
instance Negatable CubPoly where
  negify (a3,a2,a1,a0) = (-a3,-a2,-a1,-a0)
instance Zeroable CubPoly where
  zero = (0,0,0,0)
instance Unitable CubPoly where
  unit = (0,0,0,1)

evalLinear :: LinPoly -> Double -> Double
evalLinear (a1,a0) t = t*a1+a0

evalQuadratic :: QuadPoly -> Double -> Double
evalQuadratic (a2,a1,a0) t = t*(t*a2+a1)+a0

evalCubic :: CubPoly -> Double -> Double
evalCubic (a3,a2,a1,a0) t = t*(t*(t*a3+a2)+a1)+a0

derivCoeffsCubic :: CubPoly -> QuadPoly
derivCoeffsCubic (a3,a2,a1,_) = (3*a3,2*a2,a1)

derivCoeffsQuadratic :: QuadPoly -> LinPoly
derivCoeffsQuadratic (a2,a1,_) = (2*a2,a1)

-- a cubic polynomial can be specified in several ways (i.e., wrt several bases, and here they are:)
-- t^3,t^2,t,1 -- the basis used for the functions above
-- (1-t)^3, 3(1-t)^2t, 3(1-t)t^2, t^3 -- the Bezier basis
{-
I'm going to explicitly write out the matrices for the changes of bases
Bezier to standard:
[ [ -1,  3, -3, 1 ]
, [  3, -6,  3, 0 ]
, [ -3,  3,  0, 0 ]
, [  1,  0,  0, 0 ]
]
side note, might want to reorder the standard basis so that this matrix is lower triangular, but w.e.,
also this has the benefit of being symmetric, so.
(determinant 9)
Cofactor matrix of Bez->Std
[ [ 0, 0, 0, 9]
, [ 0, 0, 3, 9]
, [ 0, 3, 6, 9]
, [ 9, 9, 9, 9]
]
this is still symmetric, so the inverse is:

standard to Bezier
[ [ 0,   0,   0, 1]
, [ 0,   0, 1/3, 1]
, [ 0, 1/3, 2/3, 1]
, [ 1,   1,   1, 1]
]

Similarly, for the easier quadratic case:
Bezier to standard:
[ [  1, -2, 1]
, [ -2,  2, 0]
, [  1,  0, 0]
]
and 
standard to Bezier:
[ [ 0,   0, 1 ]
, [ 0, 1/2, 1 ]
, [ 1,   1, 1 ]
]

also should add a basis conversion for the standard interpolation basis:
basis such that ei(j/n)=delta i j

std2 -> interpolation2
[ [   0,   0, 1 ]
, [ 1/4, 1/2, 1 ]
, [   1,   1, 1 ]
]
bez2 -> interpolation2
[ [   1,   0,   0 ]
, [ 1/4, 1/2, 1/4 ]
, [   0,   0,   1 ]
]
interpolation2 -> std2
[ [  2, -4,  2 ]
, [ -3,  4, -1 ]
, [  1,  0,  0 ]
]
interpolation2 -> bez2
[ [    1,    0,    0 ]
, [ -1/2,    2, -1/2 ]
, [    0,    0,    1 ]
]

std3 -> interpolation3 
[ [    0,   0,   0, 1 ]
, [ 1/27, 1/9, 1/3, 1 ]
, [ 8/27, 4/9, 2/4, 1 ]
, [    1,   1,   1, 1 ]
]
bez3 -> interpolation3
[ [    1,   0,   0,    0 ]
, [ 8/27, 4/9, 2/9, 1/27 ]
, [ 1/27, 2/9, 4/9, 8/27 ]
, [    0,   0,   0,    1 ]
]
interpolation3 -> std3
[ [  18/7,  27/14, -54/7,  45/14 ]
, [  -3/7, -99/14,  72/7, -39/14 ]
, [ -22/7,   36/7, -18/7,    4/7 ]
, [     1,      0,     0,      0 ]
]
interpolation3 -> bez3
[ [    1,    0,    0,    0 ]
, [ -5/6,    3, -3/2,  1/3 ]
, [  1/3, -3/2,    3, -5/6 ]
, [    0,    0,    0,    1 ]
]



-}

standardToBezierBasis2 :: QuadPoly -> (Double,Double,Double)
standardToBezierBasis2 (b2,b1,b0) = (b0,(b1/2)+b0,b2+b1+b0)

standardToBezierBasis3 :: CubPoly -> (Double,Double,Double,Double)
standardToBezierBasis3 (b3,b2,b1,b0) = (b0,(b1/3)+b0,(b2/3)+((2/3)*b1)+b0,b3+b2+b1+b0)

-- composeLinLin f g ~ f . g
composeLinLin :: LinPoly -> LinPoly -> LinPoly
composeLinLin (a1,a0) (b1,b0) = (a1*b1,a1*b0+a0)

composeQuadLin :: QuadPoly -> LinPoly -> QuadPoly
composeQuadLin (a2,a1,a0) (b1,b0) =
  ( a2*b1^2
  , 2*a2*b1*b0+a1*b1
  , a2*b0^2+a1*b0+a0
  )

composeCubLin :: CubPoly -> LinPoly -> CubPoly
composeCubLin (a3,a2,a1,a0) (b1,b0) = 
  ( a3*b1^3
  , 3*a3*b1^2*b0+a2*b1^2
  , 3*a3*b1*b0^2+2*a2*b1*b0+a1*b1
  , a3*b0^3+a2*b0^2+a1*b0+a0
  )

stdToBezBasis2 = standardToBezierBasis2
stdToBezBasis3 = standardToBezierBasis3

stIntToBezBasis2 :: (Double,Double,Double) -> (Double,Double,Double)
stIntToBezBasis2 (p0,p1,p2) = (p0,2*p1 - (p0+p2)/2,p2)

stIntToBezBasis3 :: (Double,Double,Double,Double) -> (Double,Double,Double,Double)
stIntToBezBasis3 (p0,p1,p2,p3) = (p0,-5/6*p0+3*p1-3/2*p2+p3/3,p0/3-3/2*p1+3*p2-5/6*p3,p3)

interpolate2Std :: Double -> Double -> Double -> QuadPoly
interpolate2Std p0 p1 p2 = interpolate2 (0,p0) (1/2,p1) (1,p2)

interpolate3Std :: Double -> Double -> Double -> Double -> CubPoly
interpolate3Std p0 p1 p2 p3 = interpolate3 (0,p0) (1/3,p1) (2/3,p2) (1,p3)

interpolate2 :: (Double,Double) -> (Double,Double) -> (Double,Double) -> QuadPoly
interpolate2 (t0,p0) (t1,p1) (t2,p2) = 
  let
    --(t-t1)(t-t2)=t^2-(t1+t2)t+t1t2
    e0 = (1,-t1-t2,t1*t2)
    q0 = evalQuadratic e0 t0
    e1 = (1,-t0-t2,t0*t2)
    q1 = evalQuadratic e1 t1
    e2 = (1,-t0-t1,t0*t1)
    q2 = evalQuadratic e2 t2
  in
    (p0/q0)*.e0 +. (p1/q1)*.e1 +. (p2/q2)*.e2

interpolate3 :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> CubPoly
interpolate3 (t0,p0) (t1,p1) (t2,p2) (t3,p3)= 
  let
    --(t-t1)(t-t2)(t-t3)=t^3-(t1+t2+t3)t^2+(t1t2+t2t3+t1t3)t -t1t2t3
    e0 = (1,-t1-t2-t3,t1*t2+t1*t3+t2*t3,-t1*t2*t3)
    q0 = evalCubic e0 t0
    e1 = (1,-t0-t2-t3,t0*t2+t0*t3+t2*t3,-t0*t2*t3)
    q1 = evalCubic e1 t1
    e2 = (1,-t1-t0-t3,t1*t0+t1*t3+t0*t3,-t1*t0*t3)
    q2 = evalCubic e2 t2
    e3 = (1,-t0-t2-t1,t0*t2+t0*t1+t2*t1,-t0*t2*t1)
    q3 = evalCubic e3 t3
  in
    (p0/q0)*.e0 +. (p1/q1)*.e1 +. (p2/q2)*.e2 +. (p3/q3)*.e3


