--{-# LANGUAGE UndecidableInstances #-}
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

newtype AsPath a = AsPath {fromAsPath :: a}

instance Pathable a => Strokable (AsPath a) where
  stroke ss = stroke ss . toPath . fromAsPath

instance Strokable Contour where
  stroke ss cont = 
    let
      (ci,ce) = strokeContour ss cont
    in
      [ci,ce]

instance Strokable Path where
  stroke ss pth = 
    [strokePath ss pth]

instance (Strokable a) => Strokable [a] where
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

circularGaussian :: Point -> Double -> LRGBA -> LRGBA -> Gradient
circularGaussian cent r = gaussianGradientRadii cent (r,r,0)

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

orange :: LRGBA
orange = LRGBA 1 0.4 0 1

black :: LRGBA
black = LRGBA 0 0 0 1

white :: LRGBA
white = LRGBA 1 1 1 1

debugContour :: StrokeStyle -> Contour -> Picture
debugContour str cont = 
  let
    bl = strokeDistance str
  in
    [ fill (LRGBA 0.6 0 0 0.6) (stroke str{strokeDistance = bl/2} (controlPolygon cont))
    , fill (LRGBA 0 0.8 0 0.8) (stroke str cont)
    , fill (LRGBA 1 1 1 1) cont
    ]

penguin :: Picture
penguin =
  [ 
    fill
      white
        [ makeEllipseCont tol (makePoint (-20) 100) (10,6,0)
        , reverseC $ makeCircleCont tol (makePoint (-18) 100) 4
        , makeEllipseCont tol (makePoint (20) 100) (10,6,0)
        , reverseC $ makeCircleCont tol (makePoint (18) 100) 4
        ]
  , fill
      orange
      $ [ makeContour
            [ PathSeg (makePoint (-5) 85)
            , PathSeg (makePoint 0 72)
            , PathSeg (makePoint 5 85)
            ]
        , makeContour
            [ PathBez2 (makePoint (-5) (-100)) (makePoint (-15) (-95))
            , PathSeg (makePoint (-25) (-100))
            , PathBez2 (makePoint (-55) (-115)) (makePoint (-75) (-125))
            , PathSeg (makePoint (-55) (-125))
            , PathBez2 (makePoint (-5) (-125)) (makePoint (-1) (-125))
            , PathSeg (makePoint (-3) (-105))
            ]
        , makeContour
            [ PathBez2 (makePoint (5) (-100)) (makePoint (15) (-95))
            , PathSeg (makePoint (25) (-100))
            , PathBez2 (makePoint (55) (-115)) (makePoint (75) (-125))
            , PathSeg (makePoint (55) (-125))
            , PathBez2 (makePoint (5) (-125)) (makePoint (1) (-125))
            , PathSeg (makePoint (3) (-105))
            ]
        {-
        , makeContour
            [ PathBez2 (makePoint (5) (-100)) (makePoint (15) (-95))
            , PathSeg (makePoint (25) (-100))
            , PathBez2 (makePoint (35) (-105)) (makePoint (45) (-110))
            , PathSeg (makePoint (35) (-110))
            , PathBez2 (makePoint (5) (-110)) (makePoint (1) (-110))
            , PathSeg (makePoint (3) (-90))
            ]
        -}
        ]
  , fill
      (gaussianGradientRadii (makePoint 0 50) (50,20,0) orange white)
      $ [ makeEllipseCont tol (makePoint 0 (-30)) (55,80,0)
        ]
  , fill
      black
      $ [ makeEllipseCont tol (makePoint 0 (-20)) (80,100,0)
        , makeEllipseCont tol (makePoint 0 90) (45,55,0)
        ]
  , fill
      white
      $ makeRoundRect tol (makePoint 0 0) 150 200 (25,25)
  ]



