--
-- Plot.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Plot
  ( module Plot
  ) where

--import Geometry.Types
--import Geometry.CommonPaths
import Geometry
import Model
import Picture
import Stroke

defaultAxisColor :: LRGBA
defaultAxisColor = LRGBA 0.2 0.2 0.2 1

defaultTransparency :: Double
defaultTransparency = 0.8

defaultRGBs :: [(Double,Double,Double)]
defaultRGBs = 
  [ (1,0.5,0)
  , (0.8,0.2,1)
  , (0.3,0.7,0.6)
  , (0,0,1)
  , (0,1,0)
  , (1,0,0)
  , (1,1,0)
  , (1,0,1)
  , (1,1,0)
  , (1,1,1)
  ]

defaultColors :: [LRGBA]
defaultColors = cycle $ map (\(r,g,b) -> makeLRGBA r g b defaultTransparency) defaultRGBs

defaultPlotGeom = PlotGeom
  { plotArea = makeBoxSides (-5) 5 (-5) 5
  , plotMinorAxisSeparation = 1/3
  , plotBaseStrokeWidth = 0.04
  }

defaultPlot :: [PlotFn] -> Plot
defaultPlot = Plot defaultPlotGeom Nothing defaultAxisColor . zip defaultColors

emptyPlot :: Plot
emptyPlot = Plot defaultPlotGeom Nothing defaultAxisColor []

data PlotGeom = PlotGeom
  { plotArea :: Box
  , plotMinorAxisSeparation :: Double
  , plotBaseStrokeWidth :: Double -- width of major axes and graphs
  }

{-
data GeometrySpec :: GeomSpec
  { defaultArea :: Box
  , expansionRatio :: Maybe Double
  , axisSepRatio 
  }
-}

data Plot = Plot
  { plotGeom :: PlotGeom
  , bgColor :: Maybe LRGBA
  , axisColor :: LRGBA
  , funcs :: [ (LRGBA, PlotFn) ]
  }

data LFunction a = LFunction String (Double -> a)

instance Show (LFunction a) where
   show (LFunction label _) = "< function: "++label++" >"
instance Eq (LFunction a) where
   (==) (LFunction l1 _) (LFunction l2 _) = l1 == l2
instance Ord (LFunction a) where
   compare (LFunction l1 _) (LFunction l2 _) = compare l1 l2

data PlotFn
  = FunctionOfX Double (LFunction Double) -- subdivisionWidth
  | FunctionOfY Double (LFunction Double) -- subdivisionWidth
  | Parametric (Double,Double) Double (LFunction Point) -- (st,end) subdivisionWidth
  -- Implicit
  deriving (Eq,Ord,Show)

plotFnOfX :: String -> (Double -> Double) -> Double -> PlotFn
plotFnOfX l f s = FunctionOfX s (LFunction l f)

plotFnOfY :: String -> (Double -> Double) -> Double -> PlotFn
plotFnOfY l f s = FunctionOfY s (LFunction l f)

plotPara :: String -> (Double -> Point) -> (Double,Double) -> Double -> PlotFn
plotPara l f i s = Parametric i s (LFunction l f)

plotPath :: PlotGeom -> PlotFn -> Path
plotPath pd (FunctionOfX s (LFunction _ f)) = 
  let
    i = boxXInterval (plotArea pd)
  in
    buildParametrizedPath (ParamPath0 (\x -> (makePoint x (f x))) i) s
plotPath pd (FunctionOfY s (LFunction _ f)) = 
  let
    i = boxYInterval (plotArea pd)
  in
    buildParametrizedPath (ParamPath0 (\y -> (makePoint (f y) y)) i) s
plotPath pd (Parametric i s (LFunction _ f)) = buildParametrizedPath (ParamPath0 f i) s

plotPicture :: Plot -> Picture
plotPicture plot = 
  let
    pg = plotGeom plot
    sw = plotBaseStrokeWidth pg
    fax = axisColor plot
    mbg = bgColor plot
    str = strokeTestS{strokeDistance=sw/2}
    pths = fmap (\(fil,pf) -> fill fil . stroke str $ plotPath pg pf) $ funcs plot
    --bbox = unionBoxes $ fmap (bounds . snd) pths
    bbox = plotArea pg
    minaxsep = plotMinorAxisSeparation pg
  in
    pths
    ++
    [ drawAxes fax (sw/2) (sw/4) (sw/10) minaxsep bbox
    ]
    ++ 
    ( case mbg of
        Just bg -> [ fill bg $ useBox bbox makeRectangle True ]
        Nothing -> []
    )
    
drawAxes :: (Filling a) => a -> Double -> Double -> Double -> Double -> Box -> SimplePicture
drawAxes fax axMainW axMajW axMinW axMinSep box = 
  let
    strMn = strokeTestS{strokeDistance=axMainW}
    strMa = strokeTestS{strokeDistance=axMajW}
    strMi = strokeTestS{strokeDistance=axMinW}
    bl = boxLeft box
    br = boxRight box
    bt = boxTop box
    bb = boxBottom box
    str axNum = 
      ( if axNum == 0
        then strMn
        else
          if (axNum `mod` 5) == 0
          then strMa
          else strMi
      )
    horiz axNum =
      stroke 
        (str axNum)
        ( let 
            yv = fromIntegral axNum*axMinSep
          in
            makePath [ PathSeg $ makePoint bl yv ] (makePoint br yv)
        )
    vert axNum =
      stroke
        (str axNum)
        ( let 
            xv = fromIntegral axNum*axMinSep
          in
            makePath [ PathSeg $ makePoint xv bb ] (makePoint xv bt)
        )
  in
    fill fax . concat
      $
        (map vert [(ceiling (bl/axMinSep))..(floor (br/axMinSep))])
        ++
        (map horiz [(ceiling (bb/axMinSep))..(floor (bt/axMinSep))])
