{-# LANGUAGE UndecidableInstances #-}
--
-- DFunction.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module DFunction
  ( module DFunction
  ) where

import MathClasses

import Control.Category.Constrained

import Control.Lens
import Vector
import GHC.TypeNats

import Paired

import Prelude hiding ((.),id,uncurry)


data DFunction a b where
  DFunction :: (Differentiable a, Differentiable b) => (a -> b) -> (a -> LinearMap (D a) (D b)) -> DFunction a b

--makeFields ''DFunction

function :: Lens' (DFunction a b) (a -> b)
function = lens (\(DFunction f _) -> f) (\(DFunction _ df) nf -> DFunction nf df)

dFunction :: Lens' (DFunction a b) (a -> LinearMap (D a) (D b))
dFunction = lens (\(DFunction _ df) -> df) (\(DFunction f _) nf -> DFunction f nf)

instance EvaluatableClass (DFunction a b) where
  type Domain (DFunction a b) c = (a~c)
  type Codomain (DFunction a b) c = b
  evaluate (DFunction f _) = f

--evaluate :: DFunction a b -> a -> b
--evaluate (DFunction f _) = f

derivative :: DFunction a b -> a -> (LinearMap (D a) (D b))
derivative (DFunction _ df) = df

gradient :: (VectorClass n t (D b), KnownNat n) => DFunction Double b -> Double -> D b
gradient (DFunction _ df) t = evaluate (df t) 1

idF1 :: (Differentiable a, VectorClass n t (D a), KnownNat n) => DFunction a a
idF1 = DFunction id $ const id

composeF1 :: ( Differentiable a
             , Differentiable c
             , Differentiable e
             , VectorClass na ta (D a)
             , VectorClass nc tc (D c)
             , VectorClass ne te (D e)
             , KnownNat na
             , KnownNat nc
             , KnownNat ne
             --, Multiplicable (LinearMap (D c) (D e)) (LinearMap (D a) (D c)) (LinearMap (D a) (D e))
             --, LinearFromTo (D a) (D e)
             ) => DFunction c e -> DFunction a c -> DFunction a e
composeF1 fn2 fn1 = 
  let
    f1 = fn1^.function
    f1p = fn1^.dFunction
    f2 = fn2^.function
    f2p = fn2^.dFunction
  in
    DFunction
      (f2 . f1)
      (\t-> (f2p . f1 $ t) . (f1p t))

instance Category DFunction where
  type Object DFunction a = ( Differentiable a
                            , VectorClass (Dimension (D a)) (Tag (D a)) (D a)
                            , KnownNat (Dimension (D a))
                            )
  (.) = composeF1
  id = idF1

sinD :: DFunction Double Double
sinD = DFunction sin (LinearMap . CM11 . cos)

cosD :: DFunction Double Double
cosD = DFunction cos (LinearMap . CM11 . negate . sin)

type PairedDifferentiable b = 
  ( Paired b
  , Paired (D b)
  , Differentiable b
  , Differentiable (TLf b)
  , Differentiable (TRt b)
  , TLf (D b) ~ D (TLf b)
  , TRt (D b) ~ D (TRt b)
  )

{-
toPairD :: PairedDifferentiable b => DFunction b (TLf b, TRt b)
toPairD = DFunction toPair (const toPair)
-}

{-
fstD :: (Differentiable b, Differentiable c) => DFunction (b,c) b
fstD = DFunction fst (const fst)
-}

{-
sndD :: (Differentiable b, Differentiable c) => DFunction (b,c) c
sndD = DFunction snd (const snd)
-}

{-
instance ( PairedDifferentiable b
         , Differentiable a
         ) => Paired (DFunction a b) where
  type TLf (DFunction a b) = DFunction a (TLf b)
  type TRt (DFunction a b) = DFunction a (TRt b)
  toPair f = (fstD . toPairD . f, sndD . toPairD . f)
  fromPair (DFunction f df, DFunction g dg) = 
    DFunction
      (\t -> fromPair (f t, g t))
      (\t dt -> fromPair (df t dt, dg t dt))
-}

{-
sumD :: (Differentiable a, Differentiable b, Differentiable c, Summable a b c, Summable (D a) (D b) (D c))
  => DFunction (a,b) c
sumD = DFunction (uncurry (+.)) (const (uncurry (+.)))
-}

{-
productD :: ( Differentiable a
            , Differentiable b
            , Differentiable c
            , Multiplicable a b c
            , Multiplicable a (D b) (D c)
            , Multiplicable (D a) b (D c)
            ) => DFunction (a,b) c
productD = DFunction (uncurry (*.)) (\(a,b) (da,db) -> (a*.db +. da*.b))
-}

