--
-- DFunction.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module DFunction
  (
  ) where

import MathClasses

import Control.Category.Constrained

import Control.Lens

import Paired

import Prelude hiding ((.),id)

data DFunction a b where
  DFunction :: (Differentiable a, Differentiable b) => (a -> b) -> (a -> (D a -> D b)) -> DFunction a b

--makeFields ''DFunction

function :: Lens' (DFunction a b) (a -> b)
function = lens (\(DFunction f _) -> f) (\(DFunction _ df) nf -> DFunction nf df)

dFunction :: Lens' (DFunction a b) (a -> (D a -> D b))
dFunction = lens (\(DFunction _ df) -> df) (\(DFunction f _) nf -> DFunction f nf)

idF1 :: (Differentiable a) => DFunction a a
idF1 = DFunction id $ const id

composeF1 :: (Differentiable a, Differentiable c, Differentiable e) =>
  DFunction c e -> DFunction a c -> DFunction a e
composeF1 fn2 fn1 = 
  let
    f1 = fn1^.function
    f1p = fn1^.dFunction
    f2 = fn2^.function
    f2p = fn2^.dFunction
  in
    DFunction
      (f2 . f1)
      (\t -> (f2p . f1 $ t) . (f1p t))



instance Category DFunction where
  type Object DFunction a = (Differentiable a)
  (.) = composeF1
  id = idF1

dSin :: DFunction Double Double
dSin = DFunction sin (\t -> ((cos t)*))

dCos :: DFunction Double Double
dCos = DFunction cos (\t -> negate . ((sin t)*))

type PairedDifferentiable b = 
  ( Paired b
  , Paired (D b)
  , Differentiable (TLf b)
  , Differentiable (TRt b)
  , TLf (D b) ~ D (TLf b)
  , TRt (D b) ~ D (TRt b)
  )

--toPairD :: PairedDifferentiable b => DFunction 

{-
instance ( Paired b
         , Paired (D b)
         , Differentiable (TLf b)
         , Differentiable (TRt b)
         , TLf (D b) ~ D (TLf b)
         , TRt (D b) ~ D (TRt b)
         ) => Paired (DFunction a b) where
  type TLf (DFunction a b) = DFunction a (TLf b)
  type TRt (DFunction a b) = DFunction a (TRt b)
  toPair f = (fst . toPair . f, snd . toPair . f)
  fromPair (f,g) = \t -> fromPair (f t, g t)
-}


