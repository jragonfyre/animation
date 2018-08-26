module Main where

import Geometry
import Geometry.Region
import Geometry.Curve
import Model
import Stroke
import Picture
import Font
import Graphics.Text.TrueType (loadFontFile)
import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)
import Control.Monad (void)
import Control.Lens ((^.),to,re)
import Control.Lens.Iso (under,from)

import Geometry.Region.Types

import Rasterizer

regularPolygon :: Integer -> Double -> Polygon
regularPolygon n s = flip buildPolygon True . map (\i -> 
  let
    theta = 2*pi *(fromIntegral i)/(fromIntegral n)
  in
    ptFromPair (s * cos theta, s* sin theta))
  $ [0..(n-1)]

heptagon = regularPolygon 7 1.0


bezier1 :: Bezier2
bezier1 = makeBezier2 (ptFromPair (-0.7,-0.2)) (ptFromPair (1,1)) (ptFromPair (0.7,-0.8))

--regionBezier1 :: ImplicitRegion
--regionBezier1 = ImplicitRegion $ 

--mid = (halfdimen,halfdimen)

toPoint :: Double -> Double -> (Int,Int) -> Point
toPoint regionSize hdimen (i,j) =
  (regionSize/2/hdimen) *. ptFromPair (fromIntegral j-hdimen, hdimen-fromIntegral i)

--rotate :: Double -> Point -> Point
--rotate theta = under (from ptAsPair) (\(x,y) -> (x*cos theta - y*sin theta, x*sin theta + y * cos theta))

quartFunc :: Point -> Double 
quartFunc pt =
  let
    (x,y) = pt^.ptAsPair
  in
    (x^2+y^2)^2 - (x^2-y^2)


regionFunc :: Double -> Region
regionFunc tol = implicitRegion quartFunc (withinTolerance tol)

--drawableImage :: (Drawable d) => DrawData d -> d -> Int -> Double -> Image RPU RGB Double
--drawableImage 

curveImage :: Double -> (Double -> Point -> Double) -> Int -> Double -> Double -> Double -> Double -> Image RPU RGB Double
curveImage tol f dimen regionSize pixScale t r = 
  let
    g = f t -- (\x -> (f x) - t)
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
    pixWidth = (regionSize/fromIntegral dimen) 
    region = implicitRegion g $ isPositive 
    bRegion = implicitRegion g $ withinTolerance (tol)
    --sRegion = ImplicitRegion $ withinTol (tol-2*pixWidth) g
    --lRegion = ImplicitRegion $ withinTol (tol+2*pixWidth) g
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        rot = rotate r
        pt = rot *. (toPoint regionSize hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        s = icPix (makeSquare pt (pixWidth*pixScale)) 6 region
      in
        if (bRegion^.inside) pt
        then
          fmap (s*) pix 
        else
          I.PixelRGB 0 0 0
    )


{-
regionImage :: Double -> (Double -> Point -> Double) -> Int -> Double -> Double -> Double -> Image RPU RGB Double
regionImage tol f dimen regionSize t r = 
  let
    g = f t -- (\x -> (f x) - t)
    hdimen = fromIntegral $ dimen `div` 2 :: Double
    dimens = (dimen,dimen)
    pixWidth = (regionSize/fromIntegral dimen) 
    region = implicitRegion g $ withinTolerance (tol)
    sRegion = implicitRegion g $ withinTolerance (tol-2*pixWidth)
    lRegion = implicitRegion g $ withinTolerance (tol+2*pixWidth)
    color = LRGBA ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t) 1
    --renderer = msaa 6 (filledRegion (solidFill color) region)
    --sHeptagon = regularPolygon 7 (1-2*pixWidth)
    --lHeptagon = regularPolygon 7 (1+2*pixWidth)
  in
    I.makeImageR I.RPU dimens (\(i,j) -> 
      let 
        rot = rotate r 
        pt = rot *. (toPoint regionSize hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        --d = distance 0.0 heptagon pt
        aaPix = toPixel $ renderer (makeSquare pt pixWidth)
      in
        if (lRegion^.inside) pt
        then
          if (sRegion^.inside) pt
          then
            pix
          else
            aaPix
        else
          I.PixelRGB 0 0 0
    )
-}


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
        rot = rotate (2*t)
        pt = rot *. (toPoint 2.1 hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        --d = distance 0.0 heptagon pt
        s = aaPix (makeSquare pt pixWidth) 6 (heptagon^.to polygonCCurve.to closedCurveRegion)
      in
        if (lHeptagon^.to polygonCCurve.insideCurve) pt
        then
          if (sHeptagon^.to polygonCCurve.insideCurve) pt
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
        rot = rotate (2*t)
        pt = rot *. (toPoint 2.1 hdimen (i,j))
        pix = I.PixelRGB ((cos t)^2) ((sin t)^2) (0.5+cos t * sin t)
        d = (heptagon^.to polygonCCurve.re _Closed.distance) pt
        s = quadraticFalloff (d/0.03)
      in
        if (heptagon^.to polygonCCurve.insideCurve) pt
        then
          pix
        else
          fmap (s*) pix 
    )

newHeptImage :: Int -> Int -> Double -> Image RPU RGB Double
newHeptImage width height t = 
  let
    origin = (0,0)^.from ptAsPair
    ar = (fromIntegral width)/(fromIntegral height) :: Double
    box = makeBoxCenterARHeight origin ar 1.0
    rot = matrixToAffine $ rotate t
    rot2 = matrixToAffine $ rotate (-t)
    heptRegion1 = polygonRegion $ transform rot heptagon
    r1 = (sin (3*t) + 1)/2 
    g1 = (sin (10*t))^2
    b1 = (cos (2*t) + 1)/2
    sf1 = solidFill $ LRGBA (r1/2) (g1/2) (b1/2) 0.5
    sf2 = solidFill $ LRGBA (g1/2) (b1/2) (r1/2) 0.5
    heptRegion2 = polygonRegion $ transform rot2 heptagon
    hr1 = Rasterizable heptRegion1 sf1 rasterize mappend
    hr2 = Rasterizable heptRegion2 sf2 rasterize mappend
  in
    convertToImage $ renderLayered (width, height) box [hr1, hr2]
    

pt1 = makePoint (-0.7) (-0.7)
pt2 = makePoint 0.7 0.7

seg = makeSegment pt1 pt2

circ1m = makeCircle pt1 0.03
circ2m = makeCircle pt2 0.03

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
        rot = rotate t
        pt = rot *. (toPoint 2.1 hdimen (i,j))
        pixRed = I.PixelRGB 1 0 0
        pixGreen = I.PixelRGB 0 1 0
        pixBlue = I.PixelRGB 0 0 1
        d1 = (circ1m^.to (circleCurve 0.1).distance) pt
        d2 = (circ2m^.to (circleCurve 0.1).distance) pt
        d = (seg^.to segmentCurve.distance) pt
        s = falloff (d/width)
        alpha1 = falloff (d1/0.01)
        alpha2 = falloff (d2/0.01)
        g = (1-alpha1)*(1-alpha2) * s
        pixLine = I.PixelRGB alpha1 g alpha2
      in
        if (circ1m^.to circleRegion.inside) pt 
        then
          pixRed
        else
          if (circ2m^.to circleRegion.inside) pt
          then
            pixBlue
          else
            pixLine
    )

pad :: Int -> Char -> String -> String
pad n c str
  = (replicate (n-length str) c) ++ str

elliptic :: Double -> Point -> Double
elliptic t pt = 
  let
    (x,y) = pt^.ptAsPair
  in
    x^3-2*x+t-y^2

{-
writeRegion :: Double -> (Double -> Point -> Double) -> (Double,Double) -> String -> Int -> Int -> Double -> IO ()
writeRegion tol f (st,et) fname nframes size regionSize = void $ do
    sequence_ $ map (\n ->
        I.writeImage
          ("img/region-"++fname++"-"++(show n)++".png")
          $ regionImage tol f size regionSize (st + (fromIntegral n)*(et-st)/(fromIntegral nframes-1))
          $ (pi/3000 * fromIntegral n)
      )
      [0..nframes-1]
-}

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

writeNewHepts :: Int -> Int -> Int -> IO ()
writeNewHepts nframes width height = void $ do
    sequence_ $ map (\n -> do
        putStrLn $ "frame " ++ (show n)
        I.writeImage
          ("img/new-hept-"++(show n)++".png")
          (newHeptImage width height (pi / 600 * fromIntegral n))
      )
      [0..nframes-1]

writeSegs :: Double -> Int -> Int -> IO ()
writeSegs width nframes size = void $ do
    sequence_ $ map (\(n,fname,falloff) -> do
        putStrLn $ "frame " ++ (show n)
        I.writeImage
          ("img/segt-"++fname ++"-"++(show n)++".png")
          (segmentImage falloff width size (0.02 * fromIntegral n))
      )
      [(fr,fname,falloff) | fr <- [0..nframes-1], (fname,falloff) <- falloffFunctions]

qBox = makeBoxSides (-500) 2000 (-400) 1600

-- 
writeGlyph :: Int -> Font -> Char -> Fill -> IO ()
writeGlyph height font char fill =
  let
    glyph = makeGlyph font char
    bbox = bounds glyph
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
    gc = glyphPath glyph
    rendrbl = [MRasterizable gc fill scanRasterizer mappend]
    cname = case char of 
      '/' -> "forward-slash"
      '.' -> "period"
      ',' -> "comma"
      ';' -> "semicolon"
      ':' -> "colon"
      _ -> [char]
  in do
    raster <- mRenderLayered (width,height) nbox rendrbl
    I.writeImage
      ("img/char-test-"++cname++"-("++(show width)++","++(show height)++").png")
      (convertToImage raster)

writeString :: Int -> Font -> String -> Fill -> IO ()
writeString height font str fill =
  let
    --glyph = makeGlyph font char
    spath = stringToPath 300 72 font str
    bbox = bounds spath
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
    rendrbl = [MRasterizable spath fill scanRasterizer mappend]
    sname = "\""++str++"\""
  in do
    raster <- mRenderLayered (width,height) nbox rendrbl
    I.writeImage
      ("img/str-test-"++sname++"-("++(show width)++","++(show height)++").png")
      (convertToImage raster)

writeGlyphAnim :: Int -> Int -> Font -> Char -> Fill -> IO ()
writeGlyphAnim time height font char fill =
  let
    glyph = makeGlyph font char
    tsecs = (fromIntegral time)/30
    omega = 0.2 -- rotations/sec
    omegarad = omega*2*pi -- radians/sec
    rot = matrixToAffine $ rotate (omegarad*tsecs)
    gc = transform rot $ glyphPath glyph
    bbox = bounds gc
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
    cname = case char of 
      '/' -> "forward-slash"
      '.' -> "period"
      ',' -> "comma"
      ';' -> "semicolon"
      ':' -> "colon"
      _ -> [char]
    rendrbl = [MRasterizable gc fill scanRasterizer mappend]
  in do
    raster <- mRenderLayered (width,height) nbox rendrbl
    I.writeImage
      ("img/char-anim-test-frame-"++(show time)++"-"++cname++"-("++(show width)++","++(show height)++").png")
      (convertToImage raster)
    
displayClosedPath :: Int -> ClosedPath -> IO ()
displayClosedPath height cont =
  let
    bbox = bounds cont
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
    fill = solidFill $ LRGBA 0.6 0.2 0.8 1.0
    rendrbl = [MRasterizable cont fill scanRasterizer mappend]
  in do
    raster <- mRenderLayered (width,height) nbox rendrbl
    I.displayImage
      (convertToImage raster)

displayPicture :: Int -> Picture -> IO ()
displayPicture height pic =
  let
    bbox = bounds pic
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
    --fill = solidFill $ LRGBA 0.6 0.2 0.8 1.0
    --rendrbl = [MRasterizable cont fill scanRasterizer mappend]
  in do
    raster <- renderPicture (width,height) nbox pic
    I.displayImage
      (convertToImage raster)

writePicture :: Int -> String -> Picture -> IO ()
writePicture height name pic =
  let
    bbox = bounds pic
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
  in do
    raster <- renderPicture (width,height) nbox pic
    I.writeImage
      ("img/picture-"++name++"-("++(show width)++","++(show height)++").png")
      (convertToImage raster)

spiroPath :: Path
spiroPath = makeSpirograph (100,30,20) 3 10000

spiroPicture :: (Filling a, Filling b) => a -> b -> Picture
spiroPicture fo fi =
  [ fill fi $ stroke strokeTestS{strokeDistance=1} spiroPath
  , fill fo $ stroke strokeTestS{strokeDistance=3} spiroPath
  ]

plotPicture :: (Filling a, Filling b) => a -> b -> Double -> ParamPath0 -> Double -> Picture
plotPicture fgr fax sw pp pstep = 
  let
    str = strokeTestS{strokeDistance=sw/2}
    pth = buildParametrizedPath pp pstep
    bbox = bounds pth
    xax = makePath [PathSeg $ makePoint (boxLeft bbox) 0] (makePoint (boxRight bbox) 0)
    yax = makePath [PathSeg $ makePoint 0 (boxBottom bbox)] (makePoint 0 (boxTop bbox))
  in
    [ fill fgr $ stroke str pth
    , fill fax $ stroke str [xax,yax]
    ]

-- assumes rext > rint > rpen
spiroBoundingBox :: (Double, Double, Double) -> Box
spiroBoundingBox (rext,rint,rpen) =
  let
    r = rext - rint + rpen
  in
    makeBoxCenter origin r r

-- time is given by framenumber
spiroAnim :: (Filling a, Filling b) => 
  Double -> (Int -> (Double, (Int,Int,Int))) -> (Int -> a) -> (Int -> b) -> Int -> Picture 
spiroAnim bounds rparam fo fi t =
  let
    (scale,(irext,irint,irpen)) = rparam t
    rs = (scale * fromIntegral irext,scale * fromIntegral irint, scale * fromIntegral irpen)
    ept = scale * (spirographEndingPoint irext irint)
    --numSteps = 1000
    step = 3
    spiroAPath = makeSpirograph rs step ept
  in
    [ fill (fi t) $ stroke strokeTestS{strokeDistance=0.6} $ spiroAPath
    , fill (fo t) $ stroke strokeTestS{strokeDistance=3} $ spiroAPath
    , fill (LRGBA 0.2 0.2 0.2 1) $ useBox (makeBoxCenter origin bounds bounds) (makeRoundRect 1e-6) (30,20)
    ]

concreteAnim = spiroAnim 150 
  (\fnum ->
      (15/20,(200,10+fnum,fnum))
  )
  (\fnum ->
      (circularGaussian (makePoint 0 0) 100 (LRGBA 1 0.5 0.3 1) (LRGBA 1 0.26 0 1))
  )
  (\fnum ->
      (LRGBA 0 0 0 0.75)
  )

main :: IO ()
main = do
  let purpleFill = (solidFill $ LRGBA 0.5 0.2 0.7 1.0)
  font <- loadFontFile "../fonts/Caudex/Caudex-Regular.ttf"
  case font of
    Left err -> 
      putStrLn ("Error: "++err)
    Right ft -> do
      {-
      sequence_ $ map (\char -> 
        writeGlyph 500 ft char purpleFill)
        "QqWwEeRr!@#$%^&*(){}[]+=_-\\\"\':;?/.><,SsDdFfGgHhAaZzXxBb"
      sequence_ $ map (\char -> 
        writeGlyph 100 ft char purpleFill)
        "QqWwEeRr!@#$%^&*(){}[]+=_-\\\"\':;?/.><,SsDdFfGgHhAaZzXxBb"
      sequence_ $ map (\char -> 
        writeGlyph 20 ft char purpleFill)
        "QqWwEeRr!@#$%^&*(){}[]+=_-\\\"\':;?/.><,SsDdFfGgHhAaZzXxBb"
      -}
      {-
      sequence_ $ map (\t ->
        writeGlyphAnim t 150 ft 'Q' purpleFill)
        [1..200]
      -}
      --writeString 1000 ft "Hello World!" (\pt -> LRGBA ((sin ((pt^.x)/100))^2) ((cos ((pt^. x)/101))^2) 0.3 1.0)
      --writeString 1000 ft "Hiu bebisar!" (\pt -> LRGBA ((sin ((pt^.x)/100))^2) ((cos ((pt^. x)/101))^2) 0.3 1.0)
      return ()
  --writePicture 2000 "penguin" penguin
  {-
  writePicture 1000 "spirograph-test" 
    $ spiroPicture 
        (circularGaussian (makePoint 0 0) 150 (LRGBA 1 0.5 0.3 1) (LRGBA 1 0.26 0 1))
        (LRGBA 0.0 0.0 0.0 0.7)
  -}
  sequence_ $ map
    (\i -> 
      let 
        resln = 1000
      in
        writePicture resln ("spiro-anim"++(show resln)++"-frame-" ++ (show i))
          $ concreteAnim i
    )
    [0..180]

  --I.writeImage "img/circTest-main.png" $ 
  --  convertToImage . renderLayered (500,500) defaultBox $ testLayers
  {-
  rasterLarge <- mRenderLayered (2500,2000) qBox $ purpleQ
  rasterMedium <- mRenderLayered (250,200) qBox $ purpleQ
  rasterSmall <- mRenderLayered (25,20) qBox $ purpleQ
  rasterXS <- mRenderLayered (10,8) qBox $ purpleQ
  sequence_ $ map (\(name, raster) -> I.writeImage ("img/q-test-"++name++".png") (convertToImage raster))
    $ [ ( "large"
        , rasterLarge
        )
      , ( "medium"
        , rasterMedium
        )
      , ( "small"
        , rasterSmall
        )
      , ( "xs"
        , rasterXS
        )
      ]
      
  I.writeImage "img/q-test-large.png" $ 
    convertToImage rasterLarge
  I.writeImage "img/q-test-large.png" $ 
    convertToImage rasterLarge
  I.writeImage "img/q-test-large.png" $ 
    convertToImage rasterLarge
  I.writeImage "img/q-test-large.png" $ 
    convertToImage rasterLarge
  -}
  --writeNewHepts 200 200 200
  --writeSegs 0.01 200 200

