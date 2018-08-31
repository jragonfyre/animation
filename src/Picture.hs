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

-- | Type class for objects which can be stroked, essentially to unify stroking of 'Path's, 'Contour's,
--   and 'ClosedPath's
class Strokable a where
  -- | takes a 
  stroke :: StrokeStyle -> a -> ClosedPath

-- | New type to turn a 'Pathable' object into a 'Strokable' object
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

-- | 'Filling' is the 
--   class of objects that abstractly describe a filling strategy, so that they can be uniformly converted
--   to 'Fill's
class Filling a where 
  toFill :: a -> Fill

instance Filling Fill where
  toFill = id

instance Filling LRGBA where
  toFill = const

-- | Allows an @'Fillable' a@ to be converted to a 'SimplePicture' by providing a 'Filling' or 'Fill'.
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

-- | A simple picture is simply a 'Fill' and a 'ClosedPath'
data SimplePicture = SimplePicture Fill ClosedPath
  --deriving (Eq,Show,Read,Ord)

-- function from R2 -> [0,1]
-- | A 'GradientF' represents a function from the plane to the interval. See 'Gradient' for more documentation.
type GradientF = Point -> Double

-- | A 'Gradient' is a function from the plane to the interval along with two colors (specified as
--   'LRGBA's, a foreground color and background color. The color at a point in the plane is a convex 
--   combination of the foreground and background colors, with the combination factor given by the
--   'GradientF'. When the combination factor is 1, the color is the foreground color, and when it is 0, the
--   color is the background color.
data Gradient = Gradient GradientF LRGBA LRGBA

-- | clamps a double to within [0,1]
clampZO :: Double -> Double
clampZO x | x < 0 = 0
          | x > 1 = 1
          | otherwise = x

-- | Produce a Gaussian 'Gradient'. Note that Gaussian pdfs can go over 1, so we clamp the pdf to [0,1].
gaussianGradient :: Point -- ^ center of the Gaussian distribution
                 -> Matrix -- ^ Gaussian coefficient matrix
                 -> LRGBA -- ^ foreground color
                 -> LRGBA -- ^ background color
                 -> Gradient
gaussianGradient cent mat = Gradient $ \pt -> 
  let
    v = pt -. cent
    s = (transpose v) *. mat *. v
  in
    clampZO $ exp (-s^2)

-- | Produces a Gaussian 'Gradient' by taking elliptical radii and rotation data and converting that to
--   a matrix with 'ellipseRadiiToMatrix' which is then passed to 'gaussianGradient'.
gaussianGradientRadii :: Point -> (Double,Double,Double) -> LRGBA -> LRGBA -> Gradient
gaussianGradientRadii cent rs = gaussianGradient cent (ellipseRadiiToMatrix rs)

-- | Short hand for producing a circular Gaussian 'Gradient' with radius 
-- (as interpreted by 'gaussianGradientRadii') given by the 'Double' parameter.
circularGaussian :: Point -- ^ center of the Gaussian
                 -> Double -- ^ radius of the Gaussian
                 -> LRGBA -- ^ foreground color
                 -> LRGBA -- ^ background color
                 -> Gradient
circularGaussian cent r = gaussianGradientRadii cent (r,r,0)

instance Filling Gradient where 
  toFill (Gradient f c1 c2) pt =
    let 
      t = f pt 
    in
      t*.c1 +. (1-t)*.c2

instance GBounded SimplePicture where
  bounds (SimplePicture _ cp) = bounds cp

-- | This type synonym defines a 'Picture' as a collection of 'SimplePicture's. The 'SimplePicture's are
--   interpreted as the first 'SimplePicture' is the top 'SimplePicture' and the last 'SimplePicture' is the
--   bottom 'SimplePicture'
type Picture = [SimplePicture]


-- | Produces a 'Picture' for debugging a 'Contour'.
--   It draws the 'controlPolygon' of a 'Contour' in a transparent red over the 'Contour' itself drawn in green,
--   all over the fill of the 'Contour', filled in white.
debugContour :: StrokeStyle -> Contour -> Picture
debugContour str cont = 
  let
    bl = strokeDistance str
  in
    [ fill (LRGBA 0.6 0 0 0.6) (stroke str{strokeDistance = bl/2} (controlPolygon cont))
    , fill (LRGBA 0 0.8 0 0.8) (stroke str cont)
    , fill (LRGBA 1 1 1 1) cont
    ]

-- | used for 'penguin'
tol :: Double
tol = 1e-6

-- | used for 'penguin'
orange :: LRGBA
orange = LRGBA 1 0.4 0 1

-- | used for 'penguin'
black :: LRGBA
black = LRGBA 0 0 0 1

-- | used for 'penguin'
white :: LRGBA
white = LRGBA 1 1 1 1

-- | A test 'Picture' which draws a simple penguin. :)
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



