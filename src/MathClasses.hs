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

-- | class synonym 'AbGroup' to denote an abelian group type
class (Summable a a a, Zeroable a, Negatable a, Subtractable a a a) => AbGroup a where
instance (Summable a a a, Zeroable a, Negatable a, Subtractable a a a) => AbGroup a where

-- | class synonym 'Vector' to denote a vector type
class (Multiplicable Double a a, AbGroup a) => Vectorlike a where
instance (Multiplicable Double a a, AbGroup a) => Vectorlike a where

-- | class synonym to denote types that will behave ok in a Polynomial situation
class (Vectorlike a, Summable a b b) => Polynomializable a b | b -> a where

instance Polynomializable Double Double where

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

