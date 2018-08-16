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
  toFill = const

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

-- function from R2 -> [0,1]
type GradientF = Point -> Double

data Gradient = Gradient GradientF LRGBA LRGBA

gaussianGradient :: Point -> Matrix -> LRGBA -> LRGBA -> Gradient
gaussianGradient cent mat = Gradient $ \pt -> 
  let
    v = pt -. cent
    s = (transpose v) *. mat *. v
  in
    exp (-s^2)

gaussianGradientRadii :: Point -> (Double,Double,Double) -> LRGBA -> LRGBA -> Gradient
gaussianGradientRadii cent rs = gaussianGradient cent (ellipseRadiiToMatrix rs)

instance Filling Gradient where 
  toFill (Gradient f c1 c2) pt =
    let 
      t = f pt 
    in
      t*.c1 +. (1-t)*.c2

instance GBounded SimplePicture where
  bounds (SimplePicture _ cp) = bounds cp

type Picture = [SimplePicture]

tol :: Double
tol = 1e-6

penguin :: Picture
penguin =
  [ 
    fill
      (LRGBA 1 1 1 1)
      $ makeEllipseCont tol (makePoint 0 (-10)) (55,75,0)
  , fill
      (LRGBA 0 0 0 1)
      $ [ makeEllipseCont tol (makePoint 0 (-20)) (80,100,0)
        , makeEllipseCont tol (makePoint 0 90) (45,55,0)
        ]
  , fill
      (LRGBA 1 1 1 1)
      $ makeRoundRect tol (makePoint 0 0) 150 200 (25,25)
  ]



