{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses #-}
--
-- MathClasses.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module MathClasses
  ( module MathClasses
  ) where

import Data.Kind

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

-- | Represents a concrete category.
--   Might need to change the implementation of this, there's kind of a lot of options.
class EvaluatableClass a where
  type Domain a b :: Constraint
  --type Codomain a b c :: Constraint
  type Codomain a b :: *
  evaluate :: (Domain a b) => a -> b -> Codomain a b

newtype TaggedFunction a b c where
  TaggedFunction :: a -> TaggedFunction a b c

instance Evaluatable a b c => EvaluatableClass (TaggedFunction a b c) where
  type Domain (TaggedFunction a b c) d = (d~b)
  type Codomain (TaggedFunction a b c) d = c
  evaluate (TaggedFunction f) = evaluate f

class Composable a b c | a b -> c where
  infixr 7 <>.
  (<>.) :: a -> b -> c

{-
-- | type constraint synonym to represent a linear map from Vectors a to b.
--   only includes constraints on m. Constraints on a and b must be placed separately
type Linear a m b = (LinearFromTo a b, m~(LinearMap a b))

class (Vectorlike (LinearMap a b), Multiplicable (LinearMap a b) a b) => LinearFromTo a b where
  type LinearMap a b :: *

instance (Vectorlike b) => LinearFromTo Double b where
  type LinearMap Double b = b
-}

type Linear a m b = (LinearFromTo a m b)

type LinearFromTo a m b = (Vectorlike m, Multiplicable m a b)


type Evaluatable a b c = (EvaluatableClass a, Domain a b, c~Codomain a b)

instance EvaluatableClass (a -> b) where
  type Domain (a -> b) c = (a ~ c)
  type Codomain (a -> b) a = b
  evaluate = ($)

infixr 0 $.
($.) :: (Evaluatable a b c) => a -> b -> c
($.) = evaluate

newtype DerivOf a = DerivOf a

-- | This is better than before, but still less than ideal
--   Doesn't work for DFunction for example. Oh well. It at least works for Polynomials well enough for now
class ( Evaluatable a c d
      , Differentiable c
      , Differentiable d
      , Evaluatable b c e
      , Linear (D c) e (D d)
      ) => DifferentiableMorphism a c d b e | a -> b c d e where
  differentiate :: a -> b
  --derivative :: a -> c -> e
  --derivative = evaluate . differentiate

infixl 1 -/. -- differentiate
(-/.) :: (DifferentiableMorphism a c d b e) => a -> b
(-/.) = differentiate

infixl 1 $/. -- evaluate derivative
($/.) :: (DifferentiableMorphism a c d b e) => a -> c -> e
($/.) f x = (f-/.) $. x

--  THIS IS TERRIBAD. TODO FIX COMPLETELY YIKES! ok uh it's maybe not that bad, but it's still pretty bad.
--   It's designed this way to allow defining the derivative of a Polynomial to be a Polynomial again, but
--   it doesn't really support changing the domain of the polynomial to say be matrices or something.
--class DifferentiableMorphism a where
  --type DifferentiableIn a c :: Constraint
  --type DerivativeOf a c d = b | b -> a c d
  ----differentiate :: (Evaluatable a c d, DifferentiableIn a c, Differentiable c, Differentiable d) =>
    --a -> DerivativeOf a c d
  --asLinear :: DerivativeOf a c d -> c -> D c -> D d
  --differentiate = differentiateTagged . TaggedFunction
  --differentiateTagged :: TaggedFunction a c d -> b


-- | class synonym 'AbGroup' to denote an abelian group type
type AbGroup a = (Summable a a a, Zeroable a, Negatable a, Subtractable a a a)

-- | class synonym 'Rng' to denote a ring type (possibly without unit)
type Rng a = (AbGroup a, Multiplicable a a a)

-- | class synonym for a unital ring
type Ring a = (Rng a, Unitable a)

type Module a b = (Ring a, AbGroup b, Multiplicable a b b)

-- | class synonym 'Vector' to denote a vector type
type Vectorlike a = (AbGroup a, Multiplicable Double a a, Multiplicable a Double a)
--type Vectorlike a = (AbGroup a, Multiplicable Double a a, Multiplicable a Double a)

-- | class synonym to denote types that will behave ok in a Polynomial situation
type Polynomializable a b = (Vectorlike a, Summable a b b, Subtractable b b a, Subtractable b a b, Pointlike b, Pointlike a, Differentiable b, (a~(D b)), Differentiable a, (a ~ (D a)))
--class (Vectorlike a, Summable a b b, Subtractable b b a, Subtractable b a b, Pointlike b) =>
--  Polynomializable a b | b -> a where

class ( Vectorlike (D a) 
      ) =>
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

instance Zeroable () where
  zero = ()
instance Subtractable () () () where
  (-.) _ _ = ()
instance Summable () () () where
  (+.) _ _ = ()
instance Multiplicable Double () () where
  (*.) _ _ = ()
instance Multiplicable () Double () where
  (*.) _ _ = ()
instance Negatable () where
  negify _ = ()
instance Unitable () where
  unit = ()
instance Pointlike () where
  affineCombo _ _ = ()
  affineComboGen _ _ = ()
instance Differentiable () where

instance (Summable a c e, Summable b d f) => Summable (a,b) (c,d) (e,f) where
  (+.) (a1,a0) (b1,b0) = (a1+.b1,a0+.b0)
instance (Subtractable a c e, Subtractable b d f) =>
    Subtractable (a,b) (c,d) (e,f) where
  (-.) (a1,a0) (b1,b0) = (a1-.b1,a0-.b0)
instance (Multiplicable c a a, Multiplicable c b b) => 
    Multiplicable c (a,b) (a, b) where
  (*.) x (a1,a0) = (x*.a1,x*.a0)
instance (Multiplicable a Double a, Multiplicable b Double b) => 
    Multiplicable (a,b) Double (a, b) where
  (*.) (a1,a0) x = (a1*.x,a0*.x)
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
instance (Multiplicable a Double a, Multiplicable b Double b, Multiplicable c Double c) => 
    Multiplicable (a,b,c) Double (a,b,c) where
  (*.) (a2,a1,a0) x = (a2*.x,a1*.x,a0*.x)
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
instance ( Multiplicable a Double a
         , Multiplicable b Double b
         , Multiplicable c Double c
         , Multiplicable d Double d
         ) => Multiplicable (a,b,c,d) Double (a,b,c,d) where
  (*.) (a3,a2,a1,a0) x = (a3*.x,a2*.x,a1*.x,a0*.x)
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


