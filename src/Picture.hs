--
-- Picture.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Picture
  ( module Picture
  ) where

import Geometry 

import Model
import Stroke

class Strokable a where
  stroke :: StrokeStyle -> a -> ClosedPath

instance Strokable Contour where
  stroke ss cont = 
    let
      (ci,ce) = strokeContour ss cont
    in
      [ci,ce]

instance Strokable Path where
  stroke ss pth = 
    [strokePath ss pth]

instance (Foldable f,Strokable a) => Strokable (f a) where
  stroke ss f = concatMap (stroke ss) f

class Filling a where 
  toFill :: a -> Fill

instance Filling Fill where
  toFill = id

instance Filling LRGBA where
  toFill = const . Just 

class Fillable a where 
  fill :: Filling b => b -> a -> SimplePicture
  fill b = withFill (toFill b)
  withFill :: Fill -> a -> SimplePicture
  

instance Fillable Contour where
  withFill f cont = SimplePicture f [cont]

instance Fillable [Contour] where
  withFill = SimplePicture

instance Strokable a => Fillable (Stroked a) where
  withFill f (Stroked ss a) = SimplePicture f (stroke ss a)

data Stroked a = Stroked StrokeStyle a
  deriving (Eq,Show,Read,Ord)

data SimplePicture = SimplePicture Fill ClosedPath
  --deriving (Eq,Show,Read,Ord)

instance GBounded SimplePicture where
  bounds (SimplePicture _ cp) = bounds cp

type Picture = [SimplePicture]

