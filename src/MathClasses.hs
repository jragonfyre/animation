{-# LANGUAGE UndecidableInstances #-}
--
-- MathClasses.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module MathClasses
  ( module MathClasses
  ) where

-- | Class for types implementing a custom addition operator.
--   Addition should be commutative if the instance is of the form
--   @'Summable' a a a@.
--
--   Note that the class takes three arguments to allow for expressing things like abelian group actions, e.g. 
--   that of vectors on points in the affine plane.
class Summable a b c | a b -> c where
  infixl 6 +.
  (+.) :: a -> b -> c
--   A default instance is provided for instances of Num with @(+.) = (+)@
  --default (+.) :: (Num a) => a -> a -> a
  --(+.) = (+)

-- | Class for types implementing a custom subtraction operator.
--   If the type implements @'Summable' a b c@ and @'Negatable' b@, then this should satisfy:
--
-- prop> x -. y == x +. (negify y)
--   
--   Indeed, this is given as the default implementation, though it may be more efficient to implement it
--   explicitly. In particular though, note that this is likely less efficient than @('-.') = (-)@ for 'Num'
--   instances.
class Subtractable a b c | a b -> c where
  infixl 6 -.
  (-.) :: a -> b -> c
  -- | default implementation
  default (-.) :: (Summable a b c, Negatable b) => a -> b -> c
  (-.) x y = x +. negify y

-- | Class for types implementing a custom multipliction operator.
--   Again, the class takes three types to allow for (semi)group actions.
--   E.g. matrices on vectors, or affine transformations on points.
class Multiplicable a b c | a b -> c where
  infixr 7 *.
  (*.) :: a -> b -> c

-- | Class for types implementing negation. The type should be an instance of
--   @'Summable' a a a@, and @'Zeroable' a@, and should satisfy:
-- 
-- prop> x +. (negify x) == zero == (negify x) +. x
--
--   The following default instance is provided when @'Subtractable' a a a@ and @'Zeroable' a@ are defined:
--
-- > negify = (zero -.)
class Negatable a where
  -- | named to not conflict with 'negate'
  negify :: a -> a
  -- | default implementation
  default negify :: (Subtractable a a a,Zeroable a) => a -> a
  negify = ((zero::a) -.)

-- | Class for types with a zero, generally a ring or commutative monoid. A default implementation is provided
--   for instances of 'Num':
--
-- > zero = 0
class Zeroable a where
  zero :: a
  -- | default implementation
  default zero :: (Num a) => a
  zero = 0

-- | Class for types with a one, generally a ring, or (noncommutative) monoid.
--   Should satisfy the equalities (or some subset depending on what instances of 'Summable' are defined):
--
-- prop> x +. zero == x == zero +. x
--
--   A default implementation is provided
--   for instances of 'Num':
--
-- > unit = 1
class Unitable a where
  unit :: a
  -- | default implementation
  default unit :: (Num a) => a
  unit = 1

-- | Class to represent an affine point.
class Pointlike a where
  -- | 'affineCombo' should represent something like 
  --
  -- > affineCombo s (t,e) = (1-t)*s+t*e
  --
  -- Default definition provided for instances of 'Vectorlike'.
  affineCombo :: a -> (Double,a) -> a
  default affineCombo :: Vectorlike a => a -> (Double,a) -> a
  affineCombo v1 (t,v2) = (1-t)*.v1 +. t*.v2
  -- | Generalized affine combination. Default instance provided for instances of 'Vectorlike'.
  --   I feel like there should be a less efficient implementation
  --   in terms of folding 'affineCombo',
  --   but I can't quite work it out, particularly since it seems like it would be numerically unstable.
  affineComboGen :: a -> [(Double,a)] -> a
  default affineComboGen :: Vectorlike a => a -> [(Double,a)] -> a
  affineComboGen pt [] = pt
  affineComboGen pt ((t1,pt1):pts) = affineComboGenH pt t1 (t1*.pt1) pts
    where
      affineComboGenH pt tot opt [] = (1-tot)*.pt +. opt
      affineComboGenH pt tot opt ((t1,pt1):mpts) = affineComboGenH pt (tot+t1) (opt +. t1*.pt1) mpts


-- | class synonym 'AbGroup' to denote an abelian group type
type AbGroup a = (Summable a a a, Zeroable a, Negatable a, Subtractable a a a)

-- | class synonym 'Vector' to denote a vector type
type Vectorlike a = (AbGroup a, Multiplicable Double a a)

-- | class synonym to denote types that will behave ok in a Polynomial situation
type Polynomializable a b = (Vectorlike a, Summable a b b, Subtractable b b a, Subtractable b a b, Pointlike b)
--class (Vectorlike a, Summable a b b, Subtractable b b a, Subtractable b a b, Pointlike b) =>
--  Polynomializable a b | b -> a where

class (Polynomializable (D a) a) =>
  Differentiable a where
  type D a :: *
  type D a = a

--instance Polynomializable a b => Differentiable b where
--  type D b = a

instance Pointlike Double where
--instance Polynomializable Double Double where
instance Differentiable Double where

instance Summable Double Double Double where 
  (+.) = (+)
instance Subtractable Double Double Double where
  (-.) = \x y -> (x-y) -- being careful about unary negation, though I probably don't need to be,
  -- too lazy to RTFM
instance Multiplicable Double Double Double where
  (*.) = (*)
instance Negatable Double where
  negify = negate
instance Zeroable Double where
instance Unitable Double where




instance (Summable a c e, Summable b d f) => Summable (a,b) (c,d) (e,f) where
  (+.) (a1,a0) (b1,b0) = (a1+.b1,a0+.b0)
instance (Subtractable a c e, Subtractable b d f) =>
    Subtractable (a,b) (c,d) (e,f) where
  (-.) (a1,a0) (b1,b0) = (a1-.b1,a0-.b0)
instance (Multiplicable c a a, Multiplicable c b b) => 
    Multiplicable c (a,b) (a, b) where
  (*.) x (a1,a0) = (x*.a1,x*.a0)
instance (Negatable a, Negatable b) => Negatable (a,b) where
  negify (a1,a0) = (negify a1,negify a0)
instance (Zeroable a, Zeroable b) => Zeroable (a,b) where
  zero = (zero,zero)
instance (Summable a b c, Summable d e f, Summable g h i) => 
    Summable (a,d,g) (b,e,h) (c,f,i) where
  (+.) (a2,a1,a0) (b2,b1,b0) = (a2+.b2,a1+.b1,a0+.b0)
instance (Subtractable a b c, Subtractable d e f, Subtractable g h i) =>
    Subtractable (a,d,g) (b,e,h) (c,f,i) where
  (-.) (a2,a1,a0) (b2,b1,b0) = (a2-.b2,a1-.b1,a0-.b0)
instance (Multiplicable d a a, Multiplicable d b b, Multiplicable d c c) => 
    Multiplicable d (a,b,c) (a,b,c) where
  (*.) x (a2,a1,a0) = (x*.a2,x*.a1,x*.a0)
instance (Negatable a, Negatable b, Negatable c) => Negatable (a,b,c) where
  negify (a2,a1,a0) = (negify a2,negify a1,negify a0)
instance (Zeroable a, Zeroable b, Zeroable c) => Zeroable (a,b,c) where
  zero = (zero,zero,zero)
instance (Summable a b c, Summable d e f, Summable g h i, Summable j k l) => 
    Summable (a,d,g,j) (b,e,h,k) (c,f,i,l) where
  (+.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3+.b3,a2+.b2,a1+.b1,a0+.b0)
instance (Subtractable a b c, Subtractable d e f, Subtractable g h i, Subtractable j k l) =>
    Subtractable (a,d,g,j) (b,e,h,k) (c,f,i,l) where
  (-.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3-.b3,a2-.b2,a1-.b1,a0-.b0)
instance (Multiplicable e a a, Multiplicable e b b, Multiplicable e c c, Multiplicable e d d) => 
    Multiplicable e (a,b,c,d) (a,b,c,d) where
  (*.) x (a3,a2,a1,a0) = (x*.a3,x*.a2,x*.a1,x*.a0)
instance (Negatable a, Negatable b, Negatable c, Negatable d) => Negatable (a,b,c,d) where
  negify (a3,a2,a1,a0) = (negify a3,negify a2,negify a1,negify a0)
instance (Zeroable a, Zeroable b, Zeroable c, Zeroable d) => Zeroable (a,b,c,d) where
  zero = (zero,zero,zero,zero)

instance (Pointlike a, Pointlike b) => Pointlike (a,b) where
  affineCombo (a1,b1) (t,(a2,b2)) = (affineCombo a1 (t,a2),affineCombo b1 (t,b2))
  affineComboGen (a1,b1) xs =
    let
      as = map (\(t,(a,b)) -> (t,a)) xs
      bs = map (\(t,(a,b)) -> (t,b)) xs
    in
      (affineComboGen a1 as,affineComboGen b1 bs)
instance (Pointlike a, Pointlike b, Pointlike c) => Pointlike (a,b,c) where
  affineCombo (a1,b1,c1) (t,(a2,b2,c2)) = 
    ( affineCombo a1 (t,a2)
    , affineCombo b1 (t,b2)
    , affineCombo c1 (t,c2)
    )
  affineComboGen (a1,b1,c1) xs =
    let
      as = map (\(t,(a,_,_)) -> (t,a)) xs
      bs = map (\(t,(_,b,_)) -> (t,b)) xs
      cs = map (\(t,(_,_,c)) -> (t,c)) xs
    in
      ( affineComboGen a1 as
      , affineComboGen b1 bs
      , affineComboGen c1 cs
      )
instance (Pointlike a, Pointlike b, Pointlike c, Pointlike d) => Pointlike (a,b,c,d) where
  affineCombo (a1,b1,c1,d1) (t,(a2,b2,c2,d2)) = 
    ( affineCombo a1 (t,a2)
    , affineCombo b1 (t,b2)
    , affineCombo c1 (t,c2)
    , affineCombo d1 (t,d2)
    )
  affineComboGen (a1,b1,c1,d1) xs =
    let
      as = map (\(t,(a,_,_,_)) -> (t,a)) xs
      bs = map (\(t,(_,b,_,_)) -> (t,b)) xs
      cs = map (\(t,(_,_,c,_)) -> (t,c)) xs
      ds = map (\(t,(_,_,_,d)) -> (t,d)) xs
    in
      ( affineComboGen a1 as
      , affineComboGen b1 bs
      , affineComboGen c1 cs
      , affineComboGen d1 ds
      )

instance (Differentiable a, Differentiable b) => Differentiable (a,b) where
  type D (a,b) = (D a, D b)
instance (Differentiable a, Differentiable b, Differentiable c) => Differentiable (a,b,c) where
  type D (a,b,c) = (D a, D b, D c)
instance (Differentiable a, Differentiable b, Differentiable c, Differentiable d) =>
  Differentiable (a,b,c,d) where
  type D (a,b,c,d) = (D a, D b, D c, D d)


