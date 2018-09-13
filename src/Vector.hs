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
import qualified Geometry.Types as T
-- import instances
import Geometry.Affine () 
import Data.Proxy
import Data.Type.Equality
import Unsafe.Coerce
--import GHC.TypeLits
import GHC.TypeNats
import Data.Finite
--import qualified Data.Vector.Sized as V
import qualified Data.Vector.Unboxed as V
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Lens ((^.),each,(%~),(&),from)
import Control.Category.Constrained (Category)
import qualified Control.Category.Constrained as CC

import MathClasses

--newtype Const a b = Const a
--newtype Id a = Id a

{-
type family Nattify a :: Nat where
  Nattify (CVector n) = n
-}


newtype LinearMap a b = LinearMap { unLinearMap :: (CMatrix (Dimension b) (Dimension a)) }

deriving instance (KnownNat (Dimension a), KnownNat (Dimension b)) => Show (LinearMap a b)

instance Category LinearMap where
  type (Object LinearMap o) = (KnownNat (Dimension o))
  id = LinearMap unit
  (.) l2 l1 = LinearMap ((unLinearMap l2) *. (unLinearMap l1))

instance (VectorClass n t a, VectorClass m s b, KnownNat n, KnownNat m) => EvaluatableClass (LinearMap a b) where
  type Domain (LinearMap a b) c = (a~c)
  type Codomain (LinearMap a b) c = b
  evaluate (LinearMap mat) vec = convertVector @s (mat*.(convertVector @CVTag vec))

class Convertible a b where
  convert' :: a -> b

convert :: forall b a. (Convertible a b) => a -> b
convert = convert'

instance Convertible T.Vector (CVector 2) where
  convert' = CV2
instance Convertible Double (CVector 1) where
  convert' = CV1
instance Convertible () (CVector 0) where
  convert' = const CV0
instance (KnownNat n) => Convertible (GVector n) (CVector n) where
  convert' = fromVector

data CVector (n::Nat) where
  CV0 :: CVector 0
  CV1 :: !Double -> CVector 1
  CV2 :: !T.Vector -> CVector 2
  CVN :: !(GVector n) -> CVector n

instance NFData (CVector n) where
  rnf (CVN v) = rnf v
  rnf x = seq x ()

deriving instance Show (CVector n)
--deriving instance Read (CVector n)
--deriving instance Eq (CVector n)
--deriving instance Ord (CVector n)


fromListCV :: forall n. (KnownNat n) => [Double] -> Maybe (CVector n)
fromListCV xs =
  case sameNat (Proxy @n) (Proxy @0) of
    Just Refl -> Just CV0
    Nothing -> case sameNat (Proxy @n) (Proxy @1) of
      Just Refl -> case xs of 
        x:_ -> Just $ CV1 x
        [] -> Nothing
      Nothing -> case sameNat (Proxy @n) (Proxy @2) of
        Just Refl -> case xs of
          (x:y:_) -> Just $ CV2 (T.makeVector x y)
          _ -> 
            Nothing
        Nothing -> fmap CVN $ fromListVec xs

zipCV :: (Double -> Double -> Double) -> CVector n -> CVector n -> CVector n
zipCV _ (CV0) _ = CV0
zipCV f (CV1 x) (CV1 y) = CV1 (f x y)
zipCV f (CV2 x) (CV2 y) = CV2 (T.makeVector (f (x^. (T.x)) (y^. (T.x))) (f (x^. (T.y)) (y^. (T.y))))
zipCV _ (CVN _) (CV0) = CV0
zipCV f (CVN x) (CV1 y) = zipCV f (CV1 (fromVector x)) (CV1 y)
zipCV f (CV1 x) (CVN y) = zipCV f (CV1 x) (CV1 (fromVector y))
zipCV f (CVN x) (CV2 y) = zipCV f (CV2 (fromVector x)) (CV2 y)
zipCV f (CV2 x) (CVN y) = zipCV f (CV2 x) (CV2 (fromVector y))
zipCV f (CVN x) (CVN y) = CVN (zipVec f x y)

mapCV :: (Double -> Double) -> CVector n -> CVector n
mapCV _ CV0 = CV0
mapCV f (CV1 x) = (CV1 (f x))
mapCV f (CV2 v) = (CV2 (v & each %~ f))
mapCV f (CVN v) = (CVN (mapVec f v))

sumCV :: CVector n -> Double
sumCV CV0 = 0
sumCV (CV1 x) = x
sumCV (CV2 v) = (v^. T.x) + (v^. T.y)
sumCV (CVN (MkGVecU v)) = V.foldl' (+) 0 v

instance Summable (CVector n) (CVector n) (CVector n) where
  (+.) = zipCV (+)
instance Subtractable (CVector n) (CVector n) (CVector n) where
  (-.) = zipCV (-)
instance Negatable (CVector n) where
  negify = mapCV negate
instance Multiplicable Double (CVector n) (CVector n) where
  (*.) x = mapCV (x*)
instance Multiplicable (CVector n) Double (CVector n) where
  (*.) v x = mapCV (*x) v
instance Multiplicable (CVector n) (CVector n) Double where
  (*.) v w = sumCV $ zipCV (*) v w
instance (KnownNat n) => Zeroable (CVector n) where
  zero = fromVector zero
{-
  zero =
    case sameNat (Proxy @n) (Proxy @0) of
      Just Refl -> CV0
      Nothing -> case sameNat (Proxy @n) (Proxy @1) of 
        Just Refl -> CV1 0
        Nothing -> case sameNat (Proxy @n) (Proxy @2) of
          Just Refl -> CV2 zero
          Nothing -> 
            CVN zero
-}

data CVTag

instance (KnownNat n) => VectorClass n CVTag (CVector n) where
  type Dimension (CVector n) = n
  type Tag (CVector n) = CVTag
  toVector (CV0) = toVector ()
  toVector (CV1 x) = toVector x
  toVector (CV2 v) = toVector v
  toVector (CVN v) = v
  fromVector gv = case sameNat (Proxy @n) (Proxy @0) of
    Just Refl -> CV0
    Nothing -> case sameNat (Proxy @n) (Proxy @1) of
      Just Refl -> CV1 (fromVector gv)
      Nothing -> case sameNat (Proxy @n) (Proxy @2) of
        Just Refl -> CV2 (fromVector gv)
        Nothing -> CVN gv

{-
instance Multiplicable (CVector n) (CVector n) Double where
-}

data CMatrix (n::Nat) (m::Nat) where
  --CM00 :: CMatrix 0 0
  --CM01 :: CMatrix 0 1
  CM10 :: CMatrix 1 0
  CM0N :: forall n. CMatrix 0 n
  CMN0 :: forall n. CMatrix n 0
  CM0 :: forall n m. CMatrix n m
  CM11 :: !Double -> CMatrix 1 1
  CM1N :: forall n. !(CVector n) -> CMatrix 1 n
  CMN1 :: forall n. !(CVector n) -> CMatrix n 1
  CM22 :: !T.Matrix -> CMatrix 2 2
  CMNM :: forall n m. !(GMatrix n m) -> CMatrix n m

deriving instance (KnownNat n, KnownNat m) => Show (CMatrix n m)

instance NFData (CMatrix n m) where
  rnf (CM1N m) = rnf m
  rnf (CMN1 m) = rnf m
  rnf (CMNM m) = rnf m
  rnf x = x `seq` ()

zipCM :: (KnownNat n, KnownNat m) => (Double -> Double -> Double) -> CMatrix n m -> CMatrix n m -> CMatrix n m
zipCM f (CM11 x) (CM11 y) = CM11 $ f x y
zipCM f (CM1N x) (CM1N y) = CM1N (zipCV f x y)
zipCM f (CMN1 x) (CMN1 y) = CMN1 (zipCV f x y)
zipCM f (CM22 m1) (CM22 m2) = 
  let
    ((a1,c1),(b1,d1)) = m1^. T.matAsComponents
    ((a2,c2),(b2,d2)) = m2^. T.matAsComponents
  in
    CM22 $ ((f a1 a2, f c1 c2), (f b1 b2, f d1 d2))^. from T.matAsComponents
zipCM f (CMNM x) (CMNM y) = CMNM (zipMat f x y)
zipCM f (CM11 x) (CM1N y) = zipCM f (CM11 x) (CM11 (fromVector $ toVector y))
zipCM f (CM1N x) (CM11 y) = zipCM f (CM11 (fromVector $ toVector x)) (CM11 y)
zipCM f (CM11 x) (CMN1 y) = zipCM f (CM11 x) (CM11 (fromVector $ toVector y))
zipCM f (CMN1 x) (CM11 y) = zipCM f (CM11 (fromVector $ toVector x)) (CM11 y)
zipCM f (CMN1 x) (CM1N y) = zipCM f (CM11 (fromVector $ toVector x)) (CM11 (fromVector $ toVector y))
zipCM f (CMN1 x) (CMNM y) = zipCM f (CMN1 x) (CMN1 (fromVector $ toVector y))
zipCM f (CMNM x) (CM1N y) = zipCM f (CM1N (fromVector $ toVector x)) (CM1N y)
zipCM f (CMNM x) (CMN1 y) = zipCM f (CMN1 (fromVector $ toVector x)) (CMN1 y)
zipCM f CM0 m = mapCM (f 0) m
zipCM f m CM0 = mapCM (flip f 0) m
zipCM _ CM0N _ = CM0N
zipCM _ _ CM0N = CM0N
zipCM _ CM10 _ = CM10
zipCM _ _ CM10 = CM10
zipCM _ CMN0 _ = CMN0
zipCM _ _ CMN0 = CMN0


-- known nat instances needed because of CM0
mapCM :: (KnownNat n, KnownNat m) => (Double -> Double) -> CMatrix n m -> CMatrix n m
mapCM f (CM11 x) = (CM11 (f x))
mapCM f (CM1N v) = (CM1N (mapCV f v))
mapCM f (CMN1 v) = (CMN1 (mapCV f v))
mapCM f (CM22 m) = 
  let
    ((a,c),(b,d)) = m^. T.matAsComponents
  in
    CM22 $ ((f a, f c), (f b, f d))^. from T.matAsComponents
mapCM f (CMNM m) = (CMNM (mapMat f m))
mapCM f CM0 = fromVector $ replicateVec (f 0)
mapCM _ m = m


instance (KnownNat n, KnownNat m) => Summable (CMatrix n m) (CMatrix n m) (CMatrix n m) where
  (+.) = zipCM (+)
instance (KnownNat n, KnownNat m) => Subtractable (CMatrix n m) (CMatrix n m) (CMatrix n m) where
  (-.) = zipCM (-)
instance (KnownNat n, KnownNat m) => Negatable (CMatrix n m) where
  negify = mapCM negate
instance (KnownNat n, KnownNat m) => Multiplicable Double (CMatrix n m) (CMatrix n m) where
  (*.) x = mapCM (x*)
instance (KnownNat n, KnownNat m) => Multiplicable (CMatrix n m) Double (CMatrix n m) where
  (*.) v x = mapCM (*x) v
instance (KnownNat n, KnownNat m) => Zeroable (CMatrix n m) where
  zero = fromVector zero
{-
  zero = case sameNat (Proxy @n) (Proxy @0) of
    Just Refl -> CM0N
    Nothing -> case sameNat (Proxy @n) (Proxy @1) of
      Just Refl -> case sameNat (Proxy @m) (Proxy @0) of
        Just Refl -> CM10
        Nothing -> case sameNat (Proxy @m) (Proxy @1) of
          Just Refl -> CM11 0
          Nothing -> CM1N zero
      Nothing -> case 
        ( do
            sameNat (Proxy@n) (Proxy @2) 
            sameNat (Proxy @m) (Proxy @2)
        ) of
        Just Refl -> case  of 
        case sameNat (Proxy @m) (Proxy @0) of
        Just Refl -> CMN0
        Nothing -> case sameNat (Proxy @m) (Proxy @1) of
          Just Refl -> CMN1 zero
          Nothing -> CMNM zero
-}
instance (KnownNat n) => Unitable (CMatrix n n) where
  unit = case sameNat (Proxy @n) (Proxy @0) of 
    Just Refl -> CM0
    Nothing -> case sameNat (Proxy @n) (Proxy @1) of
      Just Refl -> CM11 1
      Nothing -> case sameNat (Proxy @n) (Proxy @2) of
        Just Refl -> CM22 unit
        Nothing ->
          CMNM unit
      

data CMTag (n::Nat) (m::Nat)

instance (l~(n*m),KnownNat l,KnownNat n, KnownNat m) => VectorClass l (CMTag n m) (CMatrix n m) where
  type Dimension (CMatrix n m) = (n*m)
  type Tag (CMatrix n m) = (CMTag n m)
  toVector (CM11 x) = toVector x
  toVector (CMN1 x) = toVector x
  toVector (CM1N x) = toVector x
  toVector (CMNM x) = toVector x
  toVector CM0N = toVector ()
  toVector CM10 = toVector ()
  toVector CMN0 = toVector ()
  toVector CM0 = replicateVec 0
  fromVector gv = case sameNat (Proxy @n) (Proxy @0) of
    Just Refl -> CM0 --CM0N
    Nothing -> case sameNat (Proxy @n) (Proxy @1) of
      Just Refl -> case sameNat (Proxy @m) (Proxy @0) of
        Just Refl -> CM0 --CM10
        Nothing -> case sameNat (Proxy @m) (Proxy @1) of
          Just Refl -> CM11 (fromVector gv)
          Nothing -> CM1N (fromVector gv)
      Nothing -> 
        case
          ( do
              r1 <- sameNat (Proxy @n) (Proxy @2)
              r2 <- sameNat (Proxy @m) (Proxy @2)
              return (r1,r2)
          ) of
          Just (Refl,Refl) -> CM22 (fromVector gv)
          Nothing ->
            case sameNat (Proxy @m) (Proxy @0) of
              Just Refl -> CM0 --CMN0
              Nothing -> case sameNat (Proxy @m) (Proxy @1) of
                Just Refl -> (CMN1 (fromVector gv))
                Nothing -> CMNM (fromVector gv)

instance (KnownNat n, KnownNat m) => MatrixClass n m (CMatrix n m) where
  toMatrix = fromVector . toVector
  fromMatrix = fromVector . toVector

instance (KnownNat m, KnownNat n) => Multiplicable (CMatrix n m) (CVector m) (CVector n) where
  (*.) (CM11 m) (CV1 v) = CV1 (m*v)
  (*.) (CMN1 m) (CV1 v) = m *. v
  (*.) (CM1N m) (CV1 v) = (CM11 (fromVector $ toVector m)) *. (CV1 v)
  (*.) (CMNM m) (CV1 v) = (CMN1 (fromVector $ toVector m)) *. (CV1 v)
  (*.) (CM22 m) (CV2 v) = CV2 (m*.v)
  (*.) (CM1N m) (CV2 v) = (CM1N m) *. (CVN (fromVector $ toVector v))
  (*.) (CMNM m) (CV2 v) = (CMNM m) *. (CVN (fromVector $ toVector v))
  (*.) (CM1N m) (CVN v) = CV1 $ m*.(CVN v)
  (*.) (CMNM m) (CVN v) = CVN (m*.v)
  (*.) _ _ = zero

instance (KnownNat m, KnownNat n) => Multiplicable (CVector n) (CMatrix n m) (CVector m) where
  (*.) (CV1 v) (CM11 m) = CV1 (v*m)
  (*.) (CV1 v) (CM1N m) = v*.m
  (*.) (CV1 v) (CMN1 m) = (CV1 v) *. (CM11 (fromVector $ toVector m))
  (*.) (CV1 v) (CMNM m) = (CV1 v) *. (CM1N (fromVector $ toVector m))
  (*.) (CV2 v) (CM22 m) = CV2 (v*.m)
  (*.) (CV2 v) (CMN1 m) = (CVN (fromVector $ toVector v)) *. (CMN1 m)
  (*.) (CV2 v) (CMNM m) = (CVN (fromVector $ toVector v)) *. (CMNM m)
  (*.) (CVN v) (CMN1 m) = CV1 $ (CVN v)*.m
  (*.) (CVN v) (CMNM m) = CVN (v*.m)
  (*.) _ _ = zero

instance (KnownNat n, KnownNat m, KnownNat p) => Multiplicable (CMatrix n m) (CMatrix m p) (CMatrix n p) where
  -- all ways for m = 1 (and we know it)
  (*.) (CM11 m) (CM11 n) = CM11 (m*.n)
  (*.) (CM11 m) (CM1N n) = CM1N (m*.n)
  (*.) (CM11 m) (CMNM n) = CMNM (m*.n)
  (*.) (CMN1 m) (CM11 n) = CMN1 (m*.n)
  (*.) (CMN1 m) (CM1N n) = (CMNM (convertVector @(GMatTag n 1) m)) *. (CMNM (convertVector @(GMatTag 1 p) n))
  -- todo improve?
  (*.) (CMN1 m) (CMNM n) = (CMNM (convertVector @(GMatTag n 1) m)) *. (CMNM n) -- todo improve?
  (*.) (CM1N m) (CM11 n) = CM11 ((convertVector @Double m)*.n)
  (*.) (CM1N m) (CM1N n) = CM1N ((convertVector @Double m)*.n)
  (*.) (CMNM m) (CM11 n) = CMN1 (convertVector @(CVTag) (m*.n))
  (*.) (CMNM m) (CM1N n) = (CMNM m) *. (CMNM (convertVector @(GMatTag 1 p) n)) -- todo improve?
  -- all ways for m = 2 (and we know it)
  (*.) (CM22 m) (CM22 n) = CM22 (m*.n)
  (*.) (CM1N m) (CM22 n) = (CM1N m) *. (CMNM (convertVector @(GMatTag 2 2) n))
  (*.) (CMNM m) (CM22 n) = (CMNM m) *. (CMNM (convertVector @(GMatTag 2 2) n))
  (*.) (CM22 m) (CMN1 n) = (CMNM (convertVector @(GMatTag 2 2) m)) *. (CMN1 n)
  (*.) (CM22 m) (CMNM n) = (CMNM (convertVector @(GMatTag 2 2) m)) *. (CMNM n)
  -- m is unknown
  (*.) (CM1N m) (CMNM n) = CM1N (fromVector ((toVector m)*.n))
  (*.) (CMNM m) (CMN1 n) = CMN1 (fromVector (m*.(toVector n)))
  (*.) (CMNM m) (CMNM n) = CMNM (m*.n)
  -- m = 0, or we otherwise know that one of the maps is 0
  (*.) _ _ = CM0



{-
data family CVector (n::Nat)

newtype instance CVector 0 = CV0 ()
newtype instance CVector 1 = CV1 Double
newtype instance CVector n = CVN (GVector n)
-}

{-
class CanonicalVector (n::Nat) where
  type CVector n = c | c -> n
  --CanonicalVector 0 = ()
  --CanonicalVector 1 = Double
  --CanonicalVector n = V.Vector n Double

instance CanonicalVector 0 where
  type CVector 0 = ()
instance CanonicalVector 1 where
  type CVector 1 = Double
instance (2<=n) => CanonicalVector n where
  type CVector n = GVector n
-}

{-
type family CanonicalMatrix (n::Nat) (m::Nat) where
  CanonicalMatrix 0 _ = ()
  CanonicalMatrix _ 0 = ()
  CanonicalMatrix n m = GMatrix n m
-}

-- for now we'll assume vectors are vectors of Double. I want to switch to using unboxed types eventually,
-- so yeah, needs to be nonpolymorphic for that to work I think.
class (Vectorlike v, (Dimension v)~n) => VectorClass (n::Nat) (t :: *) (v :: *) | v -> n t, n t -> v where
  type Dimension v :: Nat
  --type Dimension v = n
  type Tag v :: *
  --type Tag = t
  dimension :: KnownNat n => v -> Int
  dimension _ = fromIntegral (natVal (Proxy @n))
  toVector :: v -> GVector n
  fromVector :: GVector n -> v

convertVector :: forall s t w v n. (VectorClass n t v, VectorClass n s w) => v -> w
convertVector = fromVector . toVector

class MatrixClass (n::Nat) (m::Nat) mat | mat -> n m where
  numRows :: KnownNat n => mat -> Int
  numRows _ = fromIntegral (natVal (Proxy @n))
  numCols :: KnownNat m => mat -> Int
  numCols _ = fromIntegral (natVal (Proxy @m))
  toMatrix :: mat -> GMatrix n m
  fromMatrix :: GMatrix n m -> mat

instance MatrixClass 2 2 T.Matrix where
  toMatrix = fromVector . toVector
  fromMatrix = fromVector . toVector

newtype GVector (n :: Nat) = MkGVecU { fromGVector :: V.Vector Double }
  deriving (Show,Read,Eq,Ord,Generic)

instance NFData (GVector n)

instance VectorClass 0 () () where
  type Dimension () = 0
  type Tag () = ()
  toVector () = emptyVec
  fromVector _ = ()
instance VectorClass 1 Double Double where
  type Dimension Double = 1
  type Tag Double = Double
  toVector x = singletonVec x
  fromVector = flip indexVec 0
instance VectorClass 2 T.Vector T.Vector where
  type Dimension T.Vector = 2
  type Tag T.Vector = T.Vector
  toVector v = MkGVecU (V.fromList [v^. T.x,v^. T.y])
  fromVector v = T.makeVector (indexVec v 0) (indexVec v 1)

instance VectorClass 4 T.Matrix T.Matrix where
  type Dimension T.Matrix = 4
  type Tag T.Matrix = T.Matrix
  toVector v = MkGVecU (V.fromList [v^. T.x.T.x, v^. T.y.T.x, v^. T.x.T.y, v^. T.y.T.y])
  fromVector v = ((indexVec v 0,indexVec v 2), (indexVec v 1,indexVec v 3))^.from T.matAsComponents

emptyVec :: GVector 0
emptyVec = MkGVecU (V.empty)

singletonVec :: Double -> GVector 1
singletonVec v = MkGVecU (V.singleton v)

zipVec :: (Double -> Double -> Double) -> GVector n -> GVector n -> GVector n
zipVec f (MkGVecU v1) (MkGVecU v2) = MkGVecU $ V.zipWith f v1 v2

mapVec :: (Double -> Double) -> GVector n -> GVector n
mapVec f (MkGVecU v1) = MkGVecU $ V.map f v1

indexVec :: (KnownNat n) => GVector n -> Finite n -> Double
indexVec (MkGVecU v1) i = v1 V.! (fromIntegral i)

mkVec :: forall n. (KnownNat n) => V.Vector Double -> Maybe (GVector n)
mkVec v =
  if l == V.length v
  then
    Just (MkGVecU v)
  else
    Nothing
  where
    l = fromIntegral (natVal (Proxy @n))

fromListVec :: forall n. (KnownNat n) => [Double] -> Maybe (GVector n)
fromListVec = mkVec . V.fromList

replicateVec :: forall n. (KnownNat n) => Double -> GVector n 
replicateVec v = MkGVecU $ V.replicate l v
  where
    l = fromIntegral (natVal (Proxy @n))

generateVec :: forall n. (KnownNat n) => (Finite n -> Double) -> GVector n
generateVec f = MkGVecU $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (natVal (Proxy @n))

data GVecTag 

instance Summable (GVector n) (GVector n) (GVector n) where
  (+.) = zipVec (+)
instance Subtractable (GVector n) (GVector n) (GVector n) where
  (-.) = zipVec (-)
instance Multiplicable Double (GVector n) (GVector n) where
  (*.) x = mapVec (x*)
instance Multiplicable (GVector n) Double (GVector n) where
  (*.) v x = mapVec (*x) v
instance Multiplicable (GVector n) (GVector n) Double where
  (*.) v w = V.foldl' (+) 0 . fromGVector $ zipVec (*.) v w
instance Negatable (GVector n) where
  negify = mapVec negify
instance (KnownNat n) => Zeroable (GVector n) where
  zero = replicateVec zero
instance (KnownNat n) => VectorClass n GVecTag (GVector n) where
  type Dimension (GVector n) = n
  type Tag (GVector n) = GVecTag
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
newtype GMatrix (n::Nat) (m::Nat) = MkGMatU { gmatAsVector :: GVector (n*m) }
  deriving (Eq,Ord,Generic)

instance NFData (GMatrix n m)

instance (KnownNat m, KnownNat (n*m)) => Show (GMatrix n m) where
  show (MkGMatU v) = 
      if dimension v == 0
      then "[]"
      else
        let
          m = fromIntegral (natVal (Proxy @m))
          (r1,rs) = V.splitAt m $ fromGVector v
        in
          "[ " ++ (show r1) ++ "\n" ++ (showHelper m rs) ++ "]"
    where
      --showHelper :: forall n m. (KnownNat m, KnownNat (n*m)) => Proxy n -> V.Vector (n*m) Double -> String
      showHelper m rs = 
        if V.length rs == 0
        then ""
        else
          let
            (r1,nrs) = V.splitAt m rs
          in
            ", " ++ (show r1) ++ "\n" ++ (showHelper m nrs)


generateMat :: forall n m. (KnownNat m, KnownNat (n*m)) => (Finite n -> Finite m -> Double) -> GMatrix n m
generateMat f = MkGMatU $ generateVec @(n*m) (uncurry f . unlinearIndex)

diagonal :: forall v n t. (VectorClass n t v, KnownNat n) => v -> GMatrix n n
diagonal v = 
  let
    vec = toVector v
  in
    generateMat (\i j -> if i==j then indexVec vec i else 0)

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
indexMatrix m@(MkGMatU v) r c = indexVec v $ linearIndex r c

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
zipMat f (MkGMatU v1) (MkGMatU v2) = MkGMatU (zipVec f v1 v2)

mapMat :: (Double -> Double) -> (GMatrix n m) -> (GMatrix n m)
mapMat f (MkGMatU v1) = MkGMatU (mapVec f v1)

instance Summable (GMatrix n m) (GMatrix n m) (GMatrix n m) where
  (+.) = zipMat (+)
instance Subtractable (GMatrix n m) (GMatrix n m) (GMatrix n m) where
  (-.) = zipMat (-)
instance (KnownNat m,KnownNat n) => Multiplicable (GMatrix n m) (GVector m) (GVector n) where
  (*.) mat = toVector . (mat*.) . fromVector @m @(GMatTag m 1) @(GMatrix m 1)
instance (KnownNat m,KnownNat n) => Multiplicable (GVector n) (GMatrix n m) (GVector m) where
  (*.) vec mat = toVector . (*.mat) $ fromVector @n @(GMatTag 1 n) vec
  -- the fromVector/toVector calls are free, since mat is a newtype around a vector anyway
instance Multiplicable Double (GMatrix n m) (GMatrix n m) where
  (*.) x = mapMat (x*)
instance Multiplicable (GMatrix n m) Double (GMatrix n m) where
  (*.) v x = mapMat (*x) v
instance Negatable (GMatrix n m) where
  negify = mapMat negate
instance (KnownNat (n*m)) => Zeroable (GMatrix n m) where
  zero = MkGMatU $ replicateVec 0
instance (KnownNat n) => Unitable (GMatrix n n) where
  unit = generateMat (\i j -> if i == j then 1 else 0)

--newtype GMatVector n m (l::Nat) = GMatVector { unGMatVector :: (GMatrix n m) }

data GMatTag (n::Nat) (m::Nat)

instance (l~(n*m), KnownNat l) => VectorClass l (GMatTag n m) (GMatrix n m) where
  type Dimension (GMatrix n m) = (n*m)
  type Tag (GMatrix n m) = GMatTag n m
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
    Just Refl -> indexVec v 0
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

