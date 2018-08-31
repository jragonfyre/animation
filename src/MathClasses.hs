--
-- MathClasses.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module MathClasses
  ( module MathClasses
  ) where

class Summable a b c | a b -> c where
  infixl 6 +.
  (+.) :: a -> b -> c

class Subtractable a b c | a b -> c where
  infixl 6 -.
  (-.) :: a -> b -> c

class Multiplicable a b c | a b -> c where
  infixr 7 *.
  (*.) :: a -> b -> c

class Negatable a where
  negify :: a -> a

class Zeroable a where
  zero :: a

class Unitable a where
  unit :: a

instance Summable Double Double Double where 
  (+.) = (+)
instance Multiplicable Double Double Double where
  (*.) = (*)
instance Zeroable Double where
  zero = 0
instance Negatable Double where
  negify = negate
instance Unitable Double where
  unit = 1
instance Subtractable Double Double Double where
  (-.) = \x y -> (x-y)

