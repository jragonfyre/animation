--
-- Animatable.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Animation.Animatable
  ( Animatable (..)
  , TAnimatable (..)
  ) where


-- an animatable object is just something that depends on a parameter.
-- a takes parameter and output types
class Animatable a where
  frame :: a t b -> t -> b

instance Animatable (->) where
  frame = id

-- T stands for time
class TAnimatable a where
  tframe :: a b -> Double -> b

instance (Animatable a) => (TAnimatable (a Double)) where
  tframe = frame

