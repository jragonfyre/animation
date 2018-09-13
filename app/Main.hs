module Main where

import Geometry
import Model
import Stroke
import Picture
import Font
import Plot
import Polynomial
import MathClasses
import Vector
--import qualified Data.Vector.Sized as V
import qualified Data.Maybe as M
import Graphics.Text.TrueType (loadFontFile)
import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)
import Control.Monad (void)
import Control.Lens ((^.),to,re)
import Control.Lens.Iso (from)
import Criterion
import Criterion.Main
import Control.DeepSeq

--import Geometry.Region.Types

import Rasterizer



bezier1 :: Bezier2
bezier1 = makeBezier2 (ptFromPair (-0.7,-0.2)) (ptFromPair (1,1)) (ptFromPair (0.7,-0.8))


toPoint :: Double -> Double -> (Int,Int) -> Point
toPoint regionSize hdimen (i,j) =
  (regionSize/2/hdimen) *. ptFromPair (fromIntegral j-hdimen, hdimen-fromIntegral i)


quartFunc :: Point -> Double 
quartFunc pt =
  let
    (x,y) = pt^.ptAsPair
  in
    (x^2+y^2)^2 - (x^2-y^2)

{-
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
-}
    

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
writeNewHepts :: Int -> Int -> Int -> IO ()
writeNewHepts nframes width height = void $ do
    sequence_ $ map (\n -> do
        putStrLn $ "frame " ++ (show n)
        I.writeImage
          ("img/new-hept-"++(show n)++".png")
          (newHeptImage width height (pi / 600 * fromIntegral n))
      )
      [0..nframes-1]
-}


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

writePictureNoAA :: Int -> String -> Picture -> IO ()
writePictureNoAA height name pic =
  let
    bbox = bounds pic
    (w,h) = bbox^.dimensions.vecAsPair
    width = ceiling $ (w/h) * fromIntegral height
    nw = h*(fromIntegral width)/(fromIntegral height)
    asprat = makeVector nw h
    border = (0.05::Double) *. asprat
    nbox = makeBox ((bbox^.corner)-.border) ((1.1::Double) *. asprat)
  in do
    raster <- renderPictureNoAA (width,height) nbox pic
    I.writeImage
      ("img/picture-noaa-"++name++"-("++(show width)++","++(show height)++").png")
      (convertToImage raster)

spiroPath :: Path
spiroPath = makeSpirograph (100,30,20) 3 10000

spiroPicture :: (Filling a, Filling b) => a -> b -> Picture
spiroPicture fo fi =
  [ fill fi $ stroke strokeTestS{strokeDistance=1} spiroPath
  , fill fo $ stroke strokeTestS{strokeDistance=3} spiroPath
  ]

{-
plotPicture :: (Filling a, Filling b) => a -> b -> Double -> ParamPath0 -> Double -> Picture
plotPicture fgr fax sw pp pstep = 
  let
    str = strokeTestS{strokeDistance=sw/2}
    pth = buildParametrizedPath pp pstep
    bbox = bounds pth
    --xax = makePath [PathSeg $ makePoint (boxLeft bbox) 0] (makePoint (boxRight bbox) 0)
    --yax = makePath [PathSeg $ makePoint 0 (boxBottom bbox)] (makePoint 0 (boxTop bbox))
  in
    [ fill fgr $ stroke str pth
    --, fill fax $ stroke str [xax,yax]
    , drawAxes fax (sw/2) (sw/4) (sw/10) 0.1 bbox
    ]
-}

plotPictures :: (Filling a) => a -> Double -> [(a,ParamPath0,Double)] -> Picture
plotPictures fax sw l = 
  let
    str = strokeTestS{strokeDistance=sw/2}
    pths = fmap (\(fil,pp,pstep) -> (fil,buildParametrizedPath pp pstep)) l
    bbox = unionBoxes $ fmap (bounds . snd) pths
  in
    (fmap (\(fil,pth) -> fill fil $ stroke str pth) pths)
    ++
    [ drawAxes fax (sw/2) (sw/4) (sw/10) 0.1 bbox
    ]
    
{-
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
-}

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
  (\_ ->
      (circularGaussian (makePoint 0 0) 100 (LRGBA 1 0.5 0.3 1) (LRGBA 1 0.26 0 1))
  )
  (\_ ->
      (LRGBA 0 0 0 0.75)
  )

tPoly :: QuadPoly
tPoly = (1,2,3)

testT :: Double
testT = 2

main :: IO ()
main = do
  putStrLn "Test"
  putStrLn $ show (tPoly $. testT)
  putStrLn "Test2"
  {-
  let
    ppic 
      = plotPicture 
      . defaultPlot 
      $ [ plotFnOfX
            "sin"
            sin
            0.01
        , plotPara
            "circle"
            ( \t -> makePoint (4*(cos t)) (4*(sin t)))
            (0,2*pi)
            0.01
        , plotPara
            "ellx"
            ( \t -> makePoint (5*(cos t)) (3*(sin t)))
            (0,2*pi)
            0.01
        , plotPara
            "elly"
            ( \t -> makePoint (3*(cos t)) (5*(sin t)))
            (0,2*pi)
            0.01
        ]
  -}
  let 
    mat1 = generateMat @2 @2 (\i j -> flip indexVec (linearIndex i j) . M.fromJust $ fromListVec @4 [1,2,3,4])
    mat2 = ((1,2),(3,4))^.from matAsComponents
    mat3 = CM22 mat2
    vec1 = M.fromJust $ fromListVec @2 [1,2]
    vec2 = makeVector 1 2
    vec3 = CV2 vec2
  defaultMain
    [ bgroup
        "Linear-alg-testing"
        [ bench "fixed, Vectors" . nf ((mat2*.)::Vector -> Vector) $ vec2
        , bench "variable, GVector 2" . nf ((mat1*.)::GVector 2 -> GVector 2) $ vec1
        , bench "mixed, CVector 2" . nf ((mat3*.)::CVector 2 -> CVector 2) $ vec3
        , bench "fixed, Matrix" . nf ((mat2*.)::Matrix -> Matrix) $ mat2
        , bench "variable, GMatrix 2 2" . nf ((mat1*.)::GMatrix 2 2 -> GMatrix 2 2) $ mat1
        , bench "mixed, CMatrix 2 2" . nf ((mat3*.)::CMatrix 2 2 -> CMatrix 2 2) $ mat3
        ]
    ]
  {-
  defaultMain
    [ bgroup
        "antialiasing-testing" 
        [ bench "AA" . nfIO $ writePicture 2000 "sin-ellipses-aa" ppic
        , bench "No AA" . nfIO $ writePictureNoAA 2000 "sin-ellipses-noaa" ppic
        , bench "AA after ppic evaluated" . nfIO $ writePicture 2000 "plotting-test-sin-ellipses" ppic
        , bench "No AA control" . nfIO $ writePictureNoAA 2000 "plotting-test-sin-ellipses" ppic
        , bench "AA after ppic evaluated control" . nfIO $ writePicture 2000 "plotting-test-sin-ellipses" ppic
        , bench "No AA control control" . nfIO $ writePictureNoAA 2000 "plotting-test-sin-ellipses" ppic
        ]
    ]
  -}
  --let purpleFill = (solidFill $ LRGBA 0.5 0.2 0.7 1.0)
  font <- loadFontFile "../fonts/Caudex/Caudex-Regular.ttf"
  case font of
    Left err -> 
      putStrLn ("Error: "++err)
    Right _ -> do--ft -> do
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
  --writePictureNoAA 2000 "penguin" penguin
  {-
  writePicture 1000 "spirograph-test" 
    $ spiroPicture 
        (circularGaussian (makePoint 0 0) 150 (LRGBA 1 0.5 0.3 1) (LRGBA 1 0.26 0 1))
        (LRGBA 0.0 0.0 0.0 0.7)
  -}
  {-
  sequence_ $ map
    (\i -> 
      let 
        resln = 1000
      in
        writePicture resln ("spiro-anim"++(show resln)++"-frame-" ++ (show i))
          $ concreteAnim i
    )
    [0..180]
  -}
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

