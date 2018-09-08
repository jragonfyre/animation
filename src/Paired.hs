--
-- Paired.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Paired
  ( module Paired
  ) where

import Control.Lens

class Paired c where
  type TLf c :: *
  type TRt c :: *
  toPair :: c -> (TLf c,TRt c)
  toPair = (^.asPair)
  fromPair :: (TLf c,TRt c) -> c
  fromPair = (^.from asPair)
  asPair :: Iso' c (TLf c,TRt c)
  asPair = iso toPair fromPair

instance Paired b => Paired (a -> b) where
  type TLf (a -> b) = a -> TLf b
  type TRt (a -> b) = a -> TRt b
  toPair f = (fst . toPair . f, snd . toPair . f)
  fromPair (f,g) = \t -> fromPair (f t, g t)

