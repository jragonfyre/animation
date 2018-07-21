module Main where

import Geometry
import Model
import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)
import Control.Monad (void)

regularPolygon :: Integer -> Double -> Polygon
regularPolygon n s = flip makePolygon True . map (\i -> 
  let
    theta = 2*pi *(fromIntegral i)/(fromIntegral n)
  in
    (s * cos theta, s* sin theta))
  $ [0..(n-1)]

heptagon = regularPolygon 7 1.0

{-
icPix :: (Region r) => Point -> Double -> Integer -> RegionData r -> r -> Double
icPix c w s val r = implicitCurvePix c w w s s val r

implicitCurvePix :: (Region r) => 
  Point -> Double -> Double -> Integer -> Integer -> RegionData r -> r -> Double
implicitCurvePix c w h nv nh v r = 
  let 
    t = antialiasPixelIntensity c w h nv nh v r
  in
    (-4)*t*(t-1)

aaPix :: (Region r) => Point -> Double -> Integer -> RegionData r -> r -> Double
aaPix c w s val r = antialiasPixelIntensity c w w s s val r

antialiasPixelIntensity :: (Region r) =>
  Point -> Double -> Double -> Integer -> Integer -> RegionData r -> r -> Double
antialiasPixelIntensity center pixelWidth pixelHeight numSamplesV numSamplesH val region =
  let
    (sx,sy) = center -. (1/2) *. (pixelWidth, pixelHeight)
    nv = numSamplesV-1
    nh = numSamplesH-1
    dx = pixelWidth/fromIntegral nh
    dy = pixelWidth/fromIntegral nv
    testpts = [ (sx+(fromIntegral i)*dx,sy+(fromIntegral j)*dy) | i <- [0..nh], j <- [0..nv]]
  in
    (/(fromIntegral $ numSamplesV*numSamplesH)) 
      . fromIntegral 
      . length 
      . filter (inside val region)
      $ testpts

-}

bezier1 :: Bezier2
bezier1 = makeBezier2 (-0.7,-0.2) (1,1) (0.7,-0.8)

--regionBezier1 :: ImplicitRegion
--regionBezier1 = ImplicitRegion $ 

--mid = (halfdimen,halfdimen)

toPoint :: Double -> Double -> (Int,Int) -> (Double,Double)
toPoint regionSize hdimen (i,j) = (regionSize/2/hdimen) *. (fromIntegral j-hdimen, hdimen-fromIntegral i)

rotate :: Double -> Point -> Point
rotate theta (x,y) = (x*cos theta - y*sin theta, x*sin theta + y * cos theta)

func :: Point -> Double 
func (x,y) = (x^2+y^2)^2 - (x^2-y^2)

withinTol :: Double -> (Point -> Double) -> Point -> Bool
withinTol tol f pt = abs (f pt) < tol

positive :: (Point -> Double) -> Point -> Bool
positive f pt = f pt > 0

regionFunc :: Double -> ImplicitRegion
regionFunc tol = ImplicitRegion $ withinTol tol func

--drawableImage :: (Drawable d) => DrawData d -> d -> Int -> Double -> Image RPU RGB Double
--drawableImage 

curveImage :: Double -> (Double -> Point -> Double) -> Int -> Double -> Double -> Double -> Double -> Image RPU RGB Double
curveImage tol f dimen regionSize pixScale t r = 
  let
    g = f t -- (\x -> (f x) - t)
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
    pixWidth = (regionSize/fromIntegral dimen) 
    region = ImplicitRegion $ positive g
    bRegion = ImplicitRegion $ withinTol (tol) g
    --sRegion = ImplicitRegion $ withinTol (tol-2*pixWidth) g
    --lRegion = ImplicitRegion $ withinTol (tol+2*pixWidth) g
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        pt = rotate (r) (toPoint regionSize hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        s = icPix pt (pixWidth*pixScale) 6 () region
      in
        if inside () bRegion pt
        then
          fmap (s*) pix 
        else
          I.PixelRGB 0 0 0
    )


regionImage :: Double -> (Double -> Point -> Double) -> Int -> Double -> Double -> Double -> Image RPU RGB Double
regionImage tol f dimen regionSize t r = 
  let
    g = f t -- (\x -> (f x) - t)
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
    pixWidth = (regionSize/fromIntegral dimen) 
    region = ImplicitRegion $ withinTol (tol) g
    sRegion = ImplicitRegion $ withinTol (tol-2*pixWidth) g
    lRegion = ImplicitRegion $ withinTol (tol+2*pixWidth) g
    --sHeptagon = regularPolygon 7 (1-2*pixWidth)
    --lHeptagon = regularPolygon 7 (1+2*pixWidth)
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        pt = rotate (r) (toPoint regionSize hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        --d = distance 0.0 heptagon pt
        s = aaPix pt pixWidth 6 () region
      in
        if inside () lRegion pt
        then
          if inside () sRegion pt
          then
            pix
          else
            fmap (s*) pix 
        else
          I.PixelRGB 0 0 0
    )


aaHeptagonImage :: Int -> Double -> Image RPU RGB Double
aaHeptagonImage dimen t = 
  let
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
    pixWidth = (2.1/fromIntegral dimen) 
    sHeptagon = regularPolygon 7 (1-2*pixWidth)
    lHeptagon = regularPolygon 7 (1+2*pixWidth)
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        pt = rotate (2*t) (toPoint 2.1 hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        --d = distance 0.0 heptagon pt
        s = aaPix pt pixWidth 6 () heptagon
      in
        if inside () lHeptagon pt
        then
          if inside () sHeptagon pt
          then
            pix
          else
            fmap (s*) pix 
        else
          I.PixelRGB 0 0 0
    )

heptagonImage :: Int -> Double -> Image RPU RGB Double
heptagonImage dimen t = 
  let
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        pt = rotate (2*t) (toPoint 2.1 hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        d = distance () heptagon pt
        s = quadraticFalloff (d/0.03)
      in
        if inside () heptagon pt
        then
          pix
        else
          fmap (s*) pix 
    )

pt1 = makePoint (-0.7) (-0.7)
pt2 = makePoint 0.7 0.7

seg = makeSegment pt1 pt2

circ1 = makeCircle pt1 0.03
circ2 = makeCircle pt2 0.03

quadraticFalloff = 
  \x -> 1/(1+((2*x)^(4)))

-- a falloff function is 1 at 0 and goes to ~0 at 1, ~0 is < 0.5%
-- idk abt the 1 thing anymore.
-- these are visually of equal width
falloffFunctions :: [(String, Double->Double)]
falloffFunctions =
  [ ( "quartic"
    , \x -> 1/(1+((2*x)^(4)))
    )
  , ( "quadratic"
    , \x -> 1/(1+((2*x)^(2)))
    )
  , ( "gaussian"
    , \x -> exp $ -3*x^2
    )
  , ( "gaussian4"
    , \x -> exp $ -(2*x)^4
    )
  , ( "logistic"
    , \x -> 1/(1+ exp (15*(x-0.5)))
    )
  ]

defaultFalloff = \x -> 1/(1+((2*x)^(4)))


segmentImage :: (Double -> Double) -> Double -> Int -> Double -> Image RPU RGB Double
segmentImage falloff width dimen t = 
  let
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        --width = 0.01
        pt = rotate t (toPoint 2.1 hdimen (i,j))
        pixRed = I.PixelRGB 1 0 0
        pixGreen = I.PixelRGB 0 1 0
        pixBlue = I.PixelRGB 0 0 1
        d1 = distance 0.0 circ1 pt
        d2 = distance 0.0 circ2 pt
        d = distance () seg pt
        s = falloff (d/width)
        alpha1 = falloff (d1/0.01)
        alpha2 = falloff (d2/0.01)
        g = (1-alpha1)*(1-alpha2) * s
        pixLine = I.PixelRGB alpha1 g alpha2
      in
        if inside () circ1 pt 
        then
          pixRed
        else
          if inside () circ2 pt
          then
            pixBlue
          else
            pixLine
    )

pad :: Int -> Char -> String -> String
pad n c str
  = (replicate (n-length str) c) ++ str

elliptic :: Double -> Point -> Double
elliptic t (x,y) = x^3-2*x+t-y^2

writeRegion :: Double -> (Double -> Point -> Double) -> (Double,Double) -> String -> Int -> Int -> Double -> IO ()
writeRegion tol f (st,et) fname nframes size regionSize = void $ do
    sequence_ $ map (\n ->
        I.writeImage
          ("img/region-"++fname++"-"++(show n)++".png")
          $ regionImage tol f size regionSize (st + (fromIntegral n)*(et-st)/(fromIntegral nframes-1))
          $ (pi/3000 * fromIntegral n)
      )
      [0..nframes-1]

writeCurve :: Double -> (Double -> Point -> Double) -> (Double,Double) -> String -> Int -> Int -> Double -> Double -> IO ()
writeCurve tol f (st,et) fname nframes size regionSize pixScale = void $ do
    sequence_ $ map (\n ->
        I.writeImage
          ("img/curve-"++fname++"-"++(show n)++".png")
          $ curveImage tol f size regionSize pixScale (st + (fromIntegral n)*(et-st)/(fromIntegral nframes-1))
          $ (pi/3000 * fromIntegral n)
      )
      [0..nframes-1]

writeHepts :: Int -> Int -> IO ()
writeHepts nframes size = void $ do
    sequence_ $ map (\n ->
        I.writeImage
          ("img/hept-"++(show n)++".png")
          (aaHeptagonImage size (pi / 600 * fromIntegral n))
      )
      [0..nframes-1]

writeSegs :: Double -> Int -> Int -> IO ()
writeSegs width nframes size = void $ do
    sequence_ $ map (\(n,fname,falloff) ->
        I.writeImage
          ("img/segt-"++fname ++"-"++(show n)++".png")
          (segmentImage falloff width size (0.02 * fromIntegral n))
      )
      [(fr,fname,falloff) | fr <- [0..nframes-1], (fname,falloff) <- falloffFunctions]

main :: IO ()
main = do
  writeHepts 200 200
  --writeSegs 0.01 200 200

