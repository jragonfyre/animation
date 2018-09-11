{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise -fplugin GHC.TypeLits.KnownNat.Solver #-} 
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-} 
{-# LANGUAGE UndecidableInstances,Strict #-}
-- Silly British spelling (prolly) xD why is it spelled Normalise xD
--
-- Vector.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Vector
  ( module Vector
  ) where

import qualified Data.Tuple as Tuple
import qualified Data.List as L
import Data.Proxy
import Data.Type.Equality
import Unsafe.Coerce
--import GHC.TypeLits
import GHC.TypeNats
import Data.Finite
--import qualified Data.Vector.Sized as V
import qualified Data.Vector.Unboxed as V

import MathClasses

--newtype Const a b = Const a
--newtype Id a = Id a

{-
type family CanonicalVector (n::Nat) where
  CanonicalVector 0 = ()
  CanonicalVector 1 = Double
  CanonicalVector n = V.Vector n Double

type family CanonicalMatrix (n::Nat) (m::Nat) where
  CanonicalMatrix 0 _ = ()
  CanonicalMatrix _ 0 = ()
  CanonicalMatrix n m = GMatrix n m
-}

-- for now we'll assume vectors are vectors of Double. I want to switch to using unboxed types eventually,
-- so yeah, needs to be nonpolymorphic for that to work I think.
class (Vectorlike v) => VectorClass (n::Nat) v | v -> n where
  --type GVector (n::Nat) :: * -> *
  --type GVector (n::Nat) :: *
  dimension :: KnownNat n => v -> Int
  dimension _ = fromIntegral (natVal (Proxy @n))
  toVector :: v -> GVectro n
  fromVector :: GVector n -> v

class MatrixClass (n::Nat) (m::Nat) mat | mat -> n m where
  numRows :: KnownNat n => mat -> Int
  numRows _ = fromIntegral (natVal (Proxy @n))
  numCols :: KnownNat m => mat -> Int
  numCols _ = fromIntegral (natVal (Proxy @m))
  toMatrix :: mat -> GMatrix n m
  fromMatrix :: GMatrix n m -> mat

newtype GVector (n :: Nat) = MkGVecU { fromGVector :: V.Vector Double }
  deriving (Show,Read,Eq,Ord,Generic)

instance VectorClass 0 () where
  toVector () = V.empty
  fromVector _ = ()
instance VectorClass 1 Double where
  toVector x = V.singleton x
  fromVector = flip V.index 0

zipVec :: (Double -> Double -> Double) -> GVector n -> GVector n -> GVector n
zipVec f (MkGVecU v1) (MkGVecU v2) = MkGVecU $ V.zipWith f v1 v2

mapVec :: (Double -> Double) -> GVector n -> GVector n
mapVec f (MkGVecU v1) = MkGVecU $ fmap f v1

instance Summable (GVector n) (GVector n) (GVector n) where
  (+.) = zipVec (+)
instance Subtractable (GVector n) (GVector n) (GVector n) where
  (-.) = zipVec (-)
instance Multiplicable Double (GVector n) (GVector n) where
  (*.) x = mapVec (x*)
instance Multiplicable (GVector n) Double (GVector n) where
  (*.) v x = mapVec (*x) v
instance Multiplicable (GVector n) (GVector n) Double where
  (*.) v w = V.foldl' (+) 0 . fromGVec $ zipVec (*.) v w
instance Negatable (GVector n) where
  negify = fmap negify
instance (KnownNat n) => Zeroable (GVector n) where
  zero = V.replicate zero
instance (KnownNat n) => VectorClass n (GVector n) where
  toVector = id
  fromVector = id

{-
instance Summable a b c => Summable (V.Vector n a) (V.Vector n a) (V.Vector n a) where
  (+.) = V.zipWith (+.)
instance Subtractable a b c => Subtractable (V.Vector n a) (V.Vector n b) (V.Vector n c) where
  (-.) = V.zipWith (-.)
instance Multiplicable Double b b => Multiplicable Double (V.Vector n b) (V.Vector n b) where
  (*.) x = fmap (x*.)
instance Multiplicable b Double b => Multiplicable (V.Vector n b) Double (V.Vector n b) where
  (*.) v x = fmap (*.x) v
instance (Ring a) => Multiplicable (V.Vector n a) (V.Vector n a) a where
  (*.) v w = V.foldl' (+.) zero (V.zipWith (*.) v w)
instance Negatable a => Negatable (V.Vector n a) where
  negify = fmap negify
instance (Zeroable a,KnownNat n) => Zeroable (V.Vector n a) where
  zero = V.replicate zero
instance (KnownNat n) => VectorClass n (V.Vector n Double) where
  toVector = id
  fromVector = id
-}

-- n is # rows, m is # cols
-- assumed to be row major
newtype GMatrix (n::Nat) (m::Nat) = MkGMatU { gmatAsVector :: V.Vector (n*m) Double }
  deriving (Eq,Ord)

instance (KnownNat m, KnownNat (n*m)) => Show (GMatrix n m) where
  show (MkGMatU v) = 
      if V.length v == 0
      then "[]"
      else
        let
          m = fromIntegral (natVal (Proxy @m))
          (r1,rs) = USV.splitAt m $ V.fromSized v
        in
          "[ " ++ (show r1) ++ "\n" ++ (showHelper m rs) ++ "]"
    where
      --showHelper :: forall n m. (KnownNat m, KnownNat (n*m)) => Proxy n -> V.Vector (n*m) Double -> String
      showHelper m rs = 
        if USV.length rs == 0
        then ""
        else
          let
            (r1,nrs) = USV.splitAt m rs
          in
            ", " ++ (show r1) ++ "\n" ++ (showHelper m nrs)


generateMat :: forall n m. (KnownNat m, KnownNat (n*m)) => (Finite n -> Finite m -> Double) -> GMatrix n m
generateMat f = MkGMatU $ V.generate @(n*m) (uncurry f . unlinearIndex)

diagonal :: forall v n. (VectorClass n v, KnownNat n) => v -> GMatrix n n
diagonal v = 
  let
    vec = toVector v
  in
    generateMat (\i j -> if i==j then V.index vec i else 0)

numRowsGM :: forall n m a. (KnownNat n, Num a) => GMatrix n m -> a
numRowsGM _ = fromIntegral (natVal (Proxy @n))

numColsGM :: forall n m a. (KnownNat m, Num a) => GMatrix n m -> a
numColsGM _ = fromIntegral (natVal (Proxy @m))

linearIndex :: forall n m. KnownNat m => Finite n -> Finite m -> Finite (n*m)
linearIndex = flip $ curry combineProduct

unlinearIndex :: forall n m. KnownNat m => Finite (n*m) -> (Finite n, Finite m)
unlinearIndex = Tuple.swap . separateProduct

-- index by row, col, starting at zero tho
indexMatrix :: ( KnownNat m, KnownNat (n*m) ) => GMatrix n m -> Finite n -> Finite m -> Double
indexMatrix m@(MkGMatU v) r c = V.index v $ linearIndex r c

instance ( KnownNat m
         , KnownNat (n*m)
         , KnownNat p
         , KnownNat (m*p)
         , KnownNat (n*p)
         ) => Multiplicable (GMatrix n m) (GMatrix m p) (GMatrix n p) where
  (*.) m1 m2 = generateMat @n @p $ \i j -> 
    L.foldl'
      (+)
      0
      $ map (\k -> (indexMatrix m1 i k) * (indexMatrix m2 k j))
      $ finites

zipMat :: (Double -> Double -> Double) -> (GMatrix n m) -> (GMatrix n m) -> (GMatrix n m)
zipMat f (MkGMatU v1) (MkGMatU v2) = MkGMatU (V.zipWith f v1 v2)

mapMat :: (Double -> Double) -> (GMatrix n m) -> (GMatrix n m)
mapMat f (MkGMatU v1) = MkGMatU (fmap f v1)

instance Summable (GMatrix n m) (GMatrix n m) (GMatrix n m) where
  (+.) = zipMat (+.)
instance Subtractable (GMatrix n m) (GMatrix n m) (GMatrix n m) where
  (-.) = zipMat (-.)
instance (KnownNat m,KnownNat n) => Multiplicable (GMatrix n m) (V.Vector m Double) (V.Vector n Double) where
  (*.) mat = toVector . (mat*.) . fromVector @m @(GMatrix m 1)
  -- the fromVector/toVector calls are free, since mat is a newtype around a vector anyway
instance Multiplicable Double (GMatrix n m) (GMatrix n m) where
  (*.) x = mapMat (x*.)
instance Multiplicable (GMatrix n m) Double (GMatrix n m) where
  (*.) v x = mapMat (*.x) v
instance Negatable (GMatrix n m) where
  negify = mapMat negify
instance (KnownNat (n*m)) => Zeroable (GMatrix n m) where
  zero = MkGMatU $ V.replicate zero
instance (l~(n*m), KnownNat l) => VectorClass l (GMatrix n m) where
  toVector = gmatAsVector
  fromVector = MkGMatU

instance MatrixClass n m (GMatrix n m) where
  toMatrix = id
  fromMatrix = id

{-
type KnownDims n m = (KnownNat n, KnownNat m)

type KnownDimsNE n m = 
               ( KnownNat n
               , KnownNat m
               , KnownNat (n*m)
               , n <= (n*m)
               , m <= (n*m)
               )
-}


{-
type family GVector (n::Nat) where
  GVector 0 = ()
  GVector 1 = Double
  GVector n = V.Vector n Double

dimension :: forall n. KnownNat n => GVector n -> Int
dimension _ = fromIntegral (natVal (Proxy @n))

toVector :: forall n. KnownNat n => GVector n -> V.Vector n Double
toVector v = case sameNat (Proxy @n) (Proxy @0) of 
  Just Refl -> V.empty
  Nothing -> case sameNat (Proxy @n) (Proxy @1) of
    Just Refl -> V.singleton v
    Nothing -> unsafeCoerce v -- THIS IS SAFE, but can break when the type family is extended
    -- the reason for the unsafeCoerce is that GHC doesn't know much about closed type families, so it
    -- can't reason that if n is not 0 or 1, then it must fall into the third equation
--toVector () = V.empty
--toVector (x::Double) = V.singleton x
--toVector x = x

fromVector :: forall n. KnownNat n => V.Vector n Double -> GVector n
fromVector v = case sameNat (Proxy @n) (Proxy @0) of
  Just Refl -> ()
  Nothing -> case sameNat (Proxy @n) (Proxy @1) of
    Just Refl -> V.index v 0
    Nothing -> unsafeCoerce v
-}


--type family (LMap (n::Nat) (m::Nat)) :: *

--type instance LMap n 0 = ()
--type instance LMap n 1 = GVector n



{-
-- | with reference to 
-- https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html 
data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
  deriving Show

type family If (x :: Bool) (a::Nat) (b::Nat) :: Nat where
  If True a _ = a
  If False _ b = b

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
UnsafeMkVec xs ++ UnsafeMkVec ys = UnsafeMkVec (xs V.++ ys)

type (Min (n :: Nat) (m :: Nat)) = If (n <=? m) n m
zipVec :: Vec n a -> Vec m b -> Vec (Min n m) (a,b)
zipVec (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zip xs ys)

zipWithVec :: (a -> b -> c) -> Vec n a -> Vec m b -> Vec (Min n m) c
zipWithVec f (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zipWith f xs ys)

takeVec :: forall n m a. KnownNat n => Vec (n+m) a -> Vec n a
takeVec (UnsafeMkVec xs) = UnsafeMkVec (V.take l xs)
  where
    l = fromIntegral (natVal (Proxy @n))

dropVec :: forall n m a. KnownNat n => Vec (n+m) a -> Vec m a
dropVec (UnsafeMkVec xs) = UnsafeMkVec (V.drop l xs)
  where
    l = fromIntegral (natVal (Proxy @n))

splitVec :: forall n m a. KnownNat n => Vec (n+m) a -> (Vec n a, Vec m a)
splitVec (UnsafeMkVec xs) = (UnsafeMkVec ys, UnsafeMkVec zs)
  where
    l = fromIntegral (natVal (Proxy @n))
    (ys,zs) = V.splitAt l xs

indexVec :: Vec n a -> Finite n -> a
indexVec v i = getVector v V.! fromIntegral (getFinite i)

replicateVec :: forall n a. KnownNat n => a -> Vec n a
replicateVec x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (natVal (Proxy @n))

generateVec :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
generateVec f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (natVal (Proxy @n))

withVec :: V.Vector a -> (forall n. KnownNat n => Vec n a -> r) -> r
withVec vec = case someNatVal (fromIntegral (V.length v)) of
  SomeNat (Proxy :: Proxy m) -> f (UnsafeMkVec @m v)

exactLength :: forall n m a. (KnownNat n, KnownNat m) => Vec n a -> Maybe (Vec m a)
exactLength v = case sameNat (Proxy @n) (Proxy @m) of 
  Just Refl -> Just v -- in this case n ~ m, so we can return v
  Nothing -> Nothing



instance (KnownNat n) => Applicative (Vec n) where
  pure = replicateVec
  fs <*> vs = zipWithVec ($) fs vs

instance Functor (Vec n) where
  fmap f = UnsafeMkVec . fmap f . getVector

-}

