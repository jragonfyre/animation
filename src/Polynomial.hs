{-# LANGUAGE UndecidableInstances #-}
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

-- | Type synonym for a constant polynomial
type CPoly = ConstantPoly Double Double
-- | Type synonym for a constant polynomial generalized
type ConstantPoly a b = b
-- | Type synonym for a linear polynomial
type LinPoly = LinearPoly Double Double
-- | Type synonym for a linear polynomial generalized
type LinearPoly a b = (a,b)
-- | Type synonym for a quadratic polynomial
type QuadPoly = QuadraticPoly Double Double
-- | Type synonym for a quadratic polynomial generalized
type QuadraticPoly a b = (a,a,b)
-- | Type synonym for a cubic polynomial
type CubPoly = CubicPoly Double Double
-- | Type synonym for a cubic polynomial generalized
type CubicPoly a b = (a,a,a,b)

instance (Summable a c e, Summable b d f) => Summable (LinearPoly a b) (LinearPoly c d) (LinearPoly e f) where
  (+.) (a1,a0) (b1,b0) = (a1+.b1,a0+.b0)
instance (Subtractable a c e, Subtractable b d f) =>
    Subtractable (LinearPoly a b) (LinearPoly c d) (LinearPoly e f) where
  (-.) (a1,a0) (b1,b0) = (a1-.b1,a0-.b0)
instance (Multiplicable Double a a, Multiplicable Double b b) => 
    Multiplicable Double (LinearPoly a b) (LinearPoly a b) where
  (*.) x (a1,a0) = (x*.a1,x*.a0)
instance (Negatable a, Negatable b) => Negatable (LinearPoly a b) where
  negify (a1,a0) = (negify a1,negify a0)
instance (Zeroable a, Zeroable b) => Zeroable (LinearPoly a b) where
  zero = (zero,zero)
instance (Zeroable a, Unitable b) => Unitable (LinearPoly a b) where
  unit = (zero,unit)
instance (Summable a c e, Summable b d f) => 
    Summable (QuadraticPoly a b) (QuadraticPoly c d) (QuadraticPoly e f) where
  (+.) (a2,a1,a0) (b2,b1,b0) = (a2+.b2,a1+.b1,a0+.b0)
instance (Subtractable a c e, Subtractable b d f) =>
    Subtractable (QuadraticPoly a b) (QuadraticPoly c d) (QuadraticPoly e f) where
  (-.) (a2,a1,a0) (b2,b1,b0) = (a2-.b2,a1-.b1,a0-.b0)
instance (Multiplicable Double a a, Multiplicable Double b b) => 
    Multiplicable Double (QuadraticPoly a b) (QuadraticPoly a b) where
  (*.) x (a2,a1,a0) = (x*.a2,x*.a1,x*.a0)
instance (Negatable a, Negatable b) => Negatable (QuadraticPoly a b) where
  negify (a2,a1,a0) = (negify a2,negify a1,negify a0)
instance (Zeroable a, Zeroable b) => Zeroable (QuadraticPoly a b) where
  zero = (zero,zero,zero)
instance (Zeroable a, Unitable b) => Unitable (QuadraticPoly a b) where
  unit = (zero,zero,unit)
instance (Summable a c e, Summable b d f) => 
    Summable (CubicPoly a b) (CubicPoly c d) (CubicPoly e f) where
  (+.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3+.b3,a2+.b2,a1+.b1,a0+.b0)
instance (Subtractable a c e, Subtractable b d f) =>
    Subtractable (CubicPoly a b) (CubicPoly c d) (CubicPoly e f) where
  (-.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3-.b3,a2-.b2,a1-.b1,a0-.b0)
instance (Multiplicable Double a a, Multiplicable Double b b) => 
    Multiplicable Double (CubicPoly a b) (CubicPoly a b) where
  (*.) x (a3,a2,a1,a0) = (x*.a3,x*.a2,x*.a1,x*.a0)
instance (Negatable a, Negatable b) => Negatable (CubicPoly a b) where
  negify (a3,a2,a1,a0) = (negify a3,negify a2,negify a1,negify a0)
instance (Zeroable a, Zeroable b) => Zeroable (CubicPoly a b) where
  zero = (zero,zero,zero,zero)
instance (Zeroable a, Unitable b) => Unitable (CubicPoly a b) where
  unit = (zero,zero,zero,unit)

-- | evaluates a linear polynomial at a point 't'
evalLinear :: (Polynomializable a b) => LinearPoly a b -> Double -> b
evalLinear (a1,a0) t = t*.a1+.a0

-- | evaluates a quadratic polynomial at a point 't'
evalQuadratic :: Polynomializable a b => QuadraticPoly a b -> Double -> b
evalQuadratic (a2,a1,a0) t = t*.(t*.a2+.a1)+.a0

-- | evaluates a cubic polynomial at a point 't'
evalCubic :: (Polynomializable a b) => CubicPoly a b -> Double -> b
evalCubic (a3,a2,a1,a0) t = t*.(t*.(t*.a3+.a2)+.a1)+.a0

-- | Synonym for 'differentiateCubic'. /Deprecated./
derivCoeffsCubic :: (Polynomializable a b, Polynomializable a a) => CubicPoly a b -> QuadraticPoly a a
derivCoeffsCubic = differentiateCubic
-- | Synonym for 'differentiateQuadratic'. /Deprecated./
derivCoeffsQuadratic :: (Polynomializable a b, Polynomializable a a) => QuadraticPoly a b -> LinearPoly a a
derivCoeffsQuadratic = differentiateQuadratic

-- | Takes a cubic polynomial and produces it's derivative
differentiateCubic :: (Polynomializable a b, Polynomializable a a) => CubicPoly a b -> QuadraticPoly a a
differentiateCubic (a3,a2,a1,_) = ((3::Double)*.a3,(2::Double)*.a2,a1)

-- | takes a quadratic polynomial and produces it's derivative
differentiateQuadratic :: (Polynomializable a b, Polynomializable a a) => QuadraticPoly a b -> LinearPoly a a
differentiateQuadratic (a2,a1,_) = ((2::Double)*.a2,a1)

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
, [ 8/27, 4/9, 2/3, 1 ]
, [    1,   1,   1, 1 ]
]
bez3 -> interpolation3
[ [    1,   0,   0,    0 ]
, [ 8/27, 4/9, 2/9, 1/27 ]
, [ 1/27, 2/9, 4/9, 8/27 ]
, [    0,   0,   0,    1 ]
]
interpolation3 -> std3
[ [  -9/2,  27/2, -27/2,  9/2 ]
, [     9, -45/2,    18, -9/2 ]
, [ -11/2,     9,  -9/2,    1 ]
, [     1,     0,     0,    0 ]
]
interpolation3 -> bez3
[ [    1,    0,    0,    0 ]
, [ -5/6,    3, -3/2,  1/3 ]
, [  1/3, -3/2,    3, -5/6 ]
, [    0,    0,    0,    1 ]
]



-}
-- | Synonym for 'stdToBezBasis2'. /Deprecated./
standardToBezierBasis2 :: Polynomializable a b => QuadraticPoly a b -> (b,b,b)
standardToBezierBasis2 = stdToBezBasis2


-- | Synonym for 'stdToBezBasis3'. /Deprecated./
standardToBezierBasis3 :: Polynomializable a b => CubicPoly a b -> (b,b,b,b)
standardToBezierBasis3 = stdToBezBasis3

-- composeLinLin f g ~ f . g
-- | composes a linear polynomial f with another linear polynomial g
--   i.e. 'composeLinLin f g' is essentially 'f . g'
composeLinLin :: Polynomializable a b => LinearPoly a b -> LinPoly -> LinearPoly a b
composeLinLin (a1,a0) (b1,b0) = (b1*.a1, b0*.a1 +. a0)

-- | composes a quadratic polynomial f with a linear polynomial g
--   i.e. 'composeQuadLin f g' is essentially 'f . g'
composeQuadLin :: Polynomializable a b => QuadraticPoly a b -> LinPoly -> QuadraticPoly a b
composeQuadLin (a2,a1,a0) (b1,b0) =
  ( (b1^2) *. a2
  , (2*b1*b0) *. a2 +. b1 *. a1
  , (b0^2) *. a2 +. b0*.a1 +. a0
  )

-- | composes a cubic polynomial f with a linear polynomial g
--   i.e. 'composeCubLin f g' is essentially 'f . g'
composeCubLin :: Polynomializable a b => CubicPoly a b -> LinPoly -> CubicPoly a b
composeCubLin (a3,a2,a1,a0) (b1,b0) = 
  ( (b1^3) *. a3
  , (3*b1^2*b0) *. a3 +. (b1^2) *. a2
  , (3*b1*b0^2) *. a3 +. (2*b1*b0) *. a2 +. b1*.a1
  , (b0^3) *. a3 +. (b0^2) *. a2 +. b0 *. a1 +. a0
  )

-- | Converts the @bez2@ basis to the @std2@ basis. Basis change matrix:
--
-- > bez2 -> std2
-- > [ [  1, -2, 1]
-- > , [ -2,  2, 0]
-- > , [  1,  0, 0]
-- > ]
bezToStdBasis2 :: Polynomializable a b => (b,b,b) -> QuadraticPoly a b
bezToStdBasis2 (s,c,e) = 
  ( (e -. c) +. (s -. c)
  --( e +. (-2::Double)*.c +. s
  , (2::Double) *. (c-.s)
  , s
  )

-- | Converts the @bez3@ basis to the @std3@ basis. Basis change matrix:
--
-- > bez3 -> std3
-- > [ [ -1,  3, -3, 1 ]
-- > , [  3, -6,  3, 0 ]
-- > , [ -3,  3,  0, 0 ]
-- > , [  1,  0,  0, 0 ]
-- > ]
bezToStdBasis3 :: Polynomializable a b => (b,b,b,b) -> CubicPoly a b
bezToStdBasis3 (s,c,d,e) = 
  ( (3::Double) *. (c -. d) +. (e -. s)
  --, (3::Double) *. (s +. (-2::Double)*.c+.d)
  , (3::Double) *. ((s -. c) +. (d -. c))
  , (3::Double) *. (c -. s)
  , s
  )

--   Converts quadratic polynomial given in the standard basis, 't^2', 't', '1', to
--   the bezier basis, '(t-1)^2', '2t(t-1)', 't^2'
-- | Converts the @std2@ basis to the @bez2@ basis. Basis change matrix:
--
-- > std2 -> bez2
-- > [ [ 0,   0, 1 ]
-- > , [ 0, 1/2, 1 ]
-- > , [ 1,   1, 1 ]
-- > ]
stdToBezBasis2 :: Polynomializable a b => QuadraticPoly a b -> (b,b,b)
stdToBezBasis2 (b2,b1,b0) = 
  ( b0
  , (1/2::Double)*.b1 +. b0
  , b2 +. b1 +. b0
  )


--   Converts cubic polynomial given in the standard basis, 't^3', 't^2', 't', '1', to
--   the bezier basis, '(t-1)^3', '3t(t-1)^2', '3t^2(t-1)', 't^3'
-- | Converts the @std3@ basis to the @bez3@ basis. Basis change matrix:
--
-- > std3 -> bez3
-- > [ [ 0,   0,   0, 1]
-- > , [ 0,   0, 1/3, 1]
-- > , [ 0, 1/3, 2/3, 1]
-- > , [ 1,   1,   1, 1]
-- > ]
stdToBezBasis3 :: Polynomializable a b => CubicPoly a b -> (b,b,b,b)
stdToBezBasis3 (b3,b2,b1,b0) = 
  ( b0
  , (1/3::Double)*.b1 +. b0
  , (1/3::Double)*.b2 +. (2/3::Double)*.b1 +. b0
  , b3 +. b2 +. b1 +. b0
  )

-- | Converts the @stInt2@ basis to the @bez2@ basis. Basis change matrix:
--
-- > interpolation2 -> bez2
-- > [ [    1,    0,    0 ]
-- > , [ -1/2,    2, -1/2 ]
-- > , [    0,    0,    1 ]
-- > ]
stIntToBezBasis2 :: Pointlike b => (b,b,b) -> (b,b,b)
stIntToBezBasis2 (p0,p1,p2) = 
  ( p0
  , affineComboGen p1 [(-1/2,p0),(-1/2,p2)]
  -- (1/2::Double)*.((p1-.p0) +. (p1-.p2)) +. p1
  --, (2::Double) *. p1 +. (-1/2::Double)*.(p0+.p2)
  , p2
  )

-- | Converts the @stInt3@ basis to the @bez3@ basis. Basis change matrix:
--
-- > interpolation3 -> bez3
-- > [ [    1,    0,    0,    0 ]
-- > , [ -5/6,    3, -3/2,  1/3 ]
-- > , [  1/3, -3/2,    3, -5/6 ]
-- > , [    0,    0,    0,    1 ]
-- > ]
stIntToBezBasis3 :: Pointlike b => (b,b,b,b) -> (b,b,b,b)
stIntToBezBasis3 (p0,p1,p2,p3) = 
  ( p0
  , affineComboGen p1 [(-5/6,p0), (-3/2,p2), (1/3,p3)]
  --, (-5/6::Double)*.p0 +. (3::Double)*.p1 +. (-3/2::Double)*.p2 +. (1/3::Double)*.p3
  , affineComboGen p2 [(-5/6,p3), (-3/2,p1), (1/3,p0)]
  --, (1/3::Double)*.p0 +. (-3/2::Double)*.p1 +. (3::Double)*.p2 +. (-5/6::Double)*.p3
  , p3
  )

-- | Converts the @stInt2@ basis to the @std2@ basis. Basis change matrix:
--
-- > interpolation2 -> std2
-- > [ [  2, -4,  2 ]
-- > , [ -3,  4, -1 ]
-- > , [  1,  0,  0 ]
-- > ]
stIntToStdBasis2 :: Polynomializable a b => (b,b,b) -> QuadraticPoly a b
stIntToStdBasis2 (p0,p1,p2) = 
  ( (2::Double)*.((p0-.p1) +. (p2-.p1))
  --( 2*p0 - 4*p1 + 2*p2
  , (3::Double)*.(p1-.p0) +. (p1-.p2)
  --, -3*p0 + 4*p1 - p2
  , p0
  )

-- | Converts the @stInt3@ basis to the @std3@ basis. Basis change matrix:
--
-- > interpolation3 -> std3
-- > [ [  -9/2,  27/2, -27/2,  9/2 ]
-- > , [     9, -45/2,    18, -9/2 ]
-- > , [ -11/2,     9,  -9/2,    1 ]
-- > , [     1,     0,     0,    0 ]
-- > ]
stIntToStdBasis3 :: Polynomializable a b => (b,b,b,b) -> CubicPoly a b
stIntToStdBasis3 (p0,p1,p2,p3) =
  ( (9/2::Double)*.(p3-.p0) +. (27/2::Double)*.(p1-.p2)
  --( (-9/2::Double)*.p0 +. (27/2::Double)*.p1 +. (-27/2::Double)*.p2 +. (9/2::Double)*.p3
  --( (-9/2)*p0 + (27/2)*p1 + (-27/2)*p2 + (9/2)*p3
  , (affineComboGen p0 [ (-45/2,p1), (18,p2), (-9/2,p3) ]) -. p0
  --, (9::Double)*.p0 +. (-45/2::Double)*.p1 +. (18::Double)*.p2 +. (-9/2::Double)*.p3
  --, 9*p0 + (-45/2)*p1 + 18*p2 + (-9/2)*p3
  , (affineComboGen p0 [ (9,p1), (-9/2,p2), (1,p3) ]) -. p0
  --, (-11/2::Double)*.p0 +. (9::Double)*.p1 +. (-9/2::Double)*.p2 +. p3
  --, (-11/2)*p0 + 9*p1 + (-9/2)*p2 + p3
  , p0
  )

-- | Converts the @std2@ basis to the @stInt2@ basis. Basis change matrix:
--
-- > std2 -> interpolation2
-- > [ [   0,   0, 1 ]
-- > , [ 1/4, 1/2, 1 ]
-- > , [   1,   1, 1 ]
-- > ]
stdToStIntBasis2 :: Polynomializable a b => QuadraticPoly a b -> (b,b,b)
stdToStIntBasis2 (a2,a1,a0) = 
  ( a0
  , (1/4::Double)*.a2 +. (1/2::Double)*.a1 +. a0
  , a2 +. a1 +. a0
  )

-- | Converts the @bez2@ basis to the @stInt2@ basis. Basis change matrix:
--
-- > bez2 -> interpolation2
-- > [ [   1,   0,   0 ]
-- > , [ 1/4, 1/2, 1/4 ]
-- > , [   0,   0,   1 ]
-- > ]
bezToStIntBasis2 :: Pointlike b => (b,b,b) -> (b,b,b)
bezToStIntBasis2 (s,c,e) = 
  ( s
  , affineComboGen c [(1/4,s), (1/4,e)]
  --, (1/4::Double)*.s +. (1/2::Double)*.c +. (1/4::Double)*.e
  , e
  )

-- | Converts the @std3@ basis to the @stInt3@ basis. Basis change matrix:
--
-- > std3 -> interpolation3 
-- > [ [    0,   0,   0, 1 ]
-- > , [ 1/27, 1/9, 1/3, 1 ]
-- > , [ 8/27, 4/9, 2/3, 1 ]
-- > , [    1,   1,   1, 1 ]
-- > ]
stdToStIntBasis3 :: Polynomializable a b => CubicPoly a b -> (b,b,b,b)
stdToStIntBasis3 (a3,a2,a1,a0) = 
  ( a0
  , (1/27::Double)*.a3 +. (1/9::Double)*.a2 +. (1/3::Double)*.a1 +. a0
  , (8/27::Double)*.a3 +. (4/9::Double)*.a2 +. (2/3::Double)*.a1 +. a0
  , a3 +. a2 +. a1 +. a0
  )

-- | Converts the @bez3@ basis to the @stInt3@ basis. Basis change matrix:
--
-- > bez3 -> interpolation3
-- > [ [    1,   0,   0,    0 ]
-- > , [ 8/27, 4/9, 2/9, 1/27 ]
-- > , [ 1/27, 2/9, 4/9, 8/27 ]
-- > , [    0,   0,   0,    1 ]
-- > ]
bezToStIntBasis3 :: Pointlike b => (b,b,b,b) -> (b,b,b,b)
bezToStIntBasis3 (s,c,d,e) =
  ( s
  , affineComboGen s [ (4/9,c), (2/9,d), (1/27,e) ]
  --, (8/27::Double)*.s +. (4/9::Double)*.c +. (2/9::Double)*.d +. (1/27::Double)*.e
  , affineComboGen s [ (2/9,c), (4/9,d), (8/27,e) ]
  --, (1/27::Double)*.s +. (2/9::Double)*.c +. (4/9::Double)*.d +. (8/27::Double)*.e
  , e
  )

{-
interpolate2Std :: Double -> Double -> Double -> QuadPoly
interpolate2Std p0 p1 p2 = interpolate2 (0,p0) (1/2,p1) (1,p2)

interpolate3Std :: Double -> Double -> Double -> Double -> CubPoly
interpolate3Std p0 p1 p2 p3 = interpolate3 (0,p0) (1/3,p1) (2/3,p2) (1,p3)
-}

-- | Compute the unique quadratic polynomial whose graph passes through the three given points.
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

-- | Compute the unique cubic polynomial whose graph passes through the four given points.
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


