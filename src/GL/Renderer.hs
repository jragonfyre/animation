--
-- Renderer.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module GL.Renderer
  ( module GL.Renderer
  ) where

import Control.Applicative
import Control.Monad.IO.Class
--import Control.Monad (unless)
import System.FilePath ((</>))
--import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Data.Vector as BV
import System.Exit (exitFailure)
import System.IO
import Foreign.Ptr (nullPtr)

-- opengl libs
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GL as Raw
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.GLUtil as U

import qualified GL.Util.GLFW as W

import Geometry
import Geometry.PathBuilder (glyphQ)
import Font
import Text
import Graphics.Text.TrueType
import Control.Lens ((^.))
import GHC.Float

defaultLayoutStyle :: LayoutStyle
defaultLayoutStyle = LayoutStyle 60 2 (Justify 0.4 1 2)

wordsToLayout = take 300 . cycle $ words $  
  "The quick brown fox jumps over the lazy dog. Hello World! This is a test. :) Etaoin shrdlu. "
  ++ "My name is Jason. Wheeee words"

demoMain :: IO ()
demoMain = do
  win <- W.initialize "Drawing Test"
  efont <- loadFontFile $ fontPath </> "Caudex" </> "Caudex-Regular.ttf"
  font <- case efont of
    Left err -> do
      putStrLn $ "Font failed to load:\n" ++ err
      exitFailure
    Right ft ->
      return ft
  let 
    gstrs = fmap (buildGlyphString font 2) wordsToLayout
    laidout = layout defaultLayoutStyle origin gstrs
    conts = concat $ fmap glyphStringContours laidout
    --conts = glyphQ
    verts = convertToVertices conts
    bBox = bounds conts
  prog <- initResources verts
  W.mainLoop (draw ((V.length verts) `div` vertSize) bBox prog win) win
  W.cleanup win


initResources :: V.Vector Float -> IO Program 
initResources verts = do
  -- load and link program
  vs <- U.loadShader GL.VertexShader $ shaderPath </> "demo.v.glsl"
  tcs <- U.loadShader GL.TessControlShader $ shaderPath </> "demo.tc.glsl"
  tes <- U.loadShader GL.TessEvaluationShader $ shaderPath </> "demo.te.glsl"
  fs <- U.loadShader GL.FragmentShader $ shaderPath </> "demo.f.glsl"
  p <- U.linkShaderProgram [vs,tcs,tes,fs]
  -- setup vao
  vao <- GL.genObjectName
  buf <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just buf
  -- load vertices immutably
  V.unsafeWith verts $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (fromIntegral $ 4 * V.length verts,ptr,GL.StaticDraw)
  GL.bindVertexArrayObject $= Just vao
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor (fromIntegral vertSize) GL.Float 0 nullPtr)
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  --p <- U.linkShaderProgram [vs,tes,fs]
  --p <- U.linkShaderProgram [vs,fs]
  GL.multisample $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.stencilTest $= GL.Enabled
  GL.stencilFuncSeparate GL.FrontAndBack $= (GL.Always, 0, 0xFF)
  GL.stencilOpSeparate GL.FrontAndBack $= (GL.OpKeep, GL.OpKeep, GL.OpKeep)
  --GL.clear [GL.StencilBuffer]
  --GL.stencilFunc $= (GL.NotEqual, 0, 0xFF)
  Program p vao
    <$> GL.get (GL.uniformLocation p "baseVert")
    <*> GL.get (GL.uniformLocation p "toVP")
    <*> GL.get (GL.uniformLocation p "solidColor")
    <*> GL.get (GL.uniformLocation p "t")


draw :: Int -> Box -> Program -> GLFW.Window -> Float -> IO ()
draw nVerts bbox (Program prog vao baseUniform vpMat colorUni tIn) win t = do
  -- clear everything and get it set up
  GL.clearColor $= GL.Color4 1 1 1 1
  -- to make sure things get cleared properly, enable writing to all buffers
  -- this should also fix flickering. Should have been flickering because of wrap around once every 256 frames
  -- yup!
  GL.colorMask $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled
  GL.stencilMask $= 0xFF
  -- clear buffers
  --GL.clear [GL.ColorBuffer] -- for debug purposes
  GL.clear [GL.ColorBuffer,GL.StencilBuffer]
  (width, height) <- GLFW.getFramebufferSize win
  let
    ar = (fromIntegral width)/(fromIntegral height)
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  -- setup program
  GL.currentProgram $= Just prog
  GL.bindVertexArrayObject $= Just vao
  GL.uniform baseUniform $= baseVertex
  affineUniform vpMat (viewTransform (scaleBoxAboutCenter 1.1 1.1 bbox) ar)
  GL.uniform colorUni $= GL.Color4 (0.6::Float) 0.2 1 1
  GL.uniform tIn $= t
  GL.patchVertices $= 5
  -- stencil pass
  GL.colorMask $= GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled
  GL.stencilMask $= 0xFF
  GL.stencilFuncSeparate GL.FrontAndBack $= (GL.Always, 0, 0xFF)
  GL.stencilOpSeparate GL.Front $= (GL.OpKeep, GL.OpKeep, GL.OpIncrWrap)
  GL.stencilOpSeparate GL.Back $= (GL.OpKeep, GL.OpKeep, GL.OpDecrWrap)
  GL.drawArrays GL.Patches 0 (fromIntegral $ nVerts)
  -- draw pass
  GL.colorMask $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled
  GL.stencilMask $= 0x00
  GL.stencilFuncSeparate GL.FrontAndBack $= (GL.Notequal, 0, 0xFF)
  GL.stencilOpSeparate GL.FrontAndBack $= (GL.OpKeep, GL.OpKeep, GL.OpKeep)
  GL.drawArrays GL.Patches 0 (fromIntegral $ nVerts)
  -- end of drawing
  GL.bindVertexArrayObject $= Nothing



-- | Represents the shader program and its input parameter
data Program = Program GL.Program GL.VertexArrayObject GL.UniformLocation GL.UniformLocation GL.UniformLocation GL.UniformLocation

shaderPath :: FilePath
shaderPath = ".." </> "shaders" </> "demo"

fontPath = ".." </> "fonts"

affineUniform :: MonadIO m => GL.UniformLocation -> Affine -> m ()
affineUniform (GL.UniformLocation ul) aff = 
  let
    (mat,trans) = aff^.affAsPair
    ((a,c),(b,d)) = mat^.matAsComponents
    (tx,ty) = trans^.vecAsPair
    [af,cf,bf,df,txf,tyf] = fmap double2Float [a,c,b,d,tx,ty]
  in
    liftIO $ V.unsafeWith 
      ( V.fromList 
        [  af,  cf, 0
        ,  bf,  df, 0
        , txf, tyf, 1
        ] ) 
      $ Raw.glUniformMatrix3fv ul 1 0

{-
viewBox :: Box
viewBox = scaleBoxAboutCenter (bounds glyphQ) 1.1 1.1
-}

viewTransform :: Box -> Double -> Affine
viewTransform vbox ar = transformBoxToBox (smallestCCBoxOfAR vbox ar) standardGLBox

baseVertex :: GL.Vector2 Float
baseVertex = GL.Vector2 0 0

vertSize :: Int
vertSize = 3

standardGLBox :: Box 
standardGLBox = makeBoxSides (-1) 1 (-1) 1

wholeSegToVertices :: Point -> WholePathSegment -> V.Vector Float
wholeSegToVertices bp (WPathSeg seg) =
  let
    sx = double2Float $ seg^.start.x
    sy = double2Float $ seg^.start.y
    ex = double2Float $ seg^.end.x
    ey = double2Float $ seg^.end.y
    bpx = double2Float $ bp^.x
    bpy = double2Float $ bp^.y
  in
    V.fromList
      [ sx, sy, 1
      , sx, sy, 1
      , ex, ey, 1
      , ex, ey, 1
      , bpx, bpy, 1
      ]
wholeSegToVertices bp (WPathBez2 bez) = wholeSegToVertices bp (WPathRBez3 (bez2ToRBez3 bez))
wholeSegToVertices bp (WPathBez3 bez) = wholeSegToVertices bp (WPathRBez3 (bez3ToRBez3 bez))
wholeSegToVertices bp (WPathRBez2 bez) = wholeSegToVertices bp (WPathRBez3 (rbez2ToRBez3 bez))
wholeSegToVertices bp (WPathRBez3 bez) = 
  let
    (s,sw) = rstart3 bez
    (c,cw) = rstCont3 bez
    (d,dw) = rendCont3 bez
    (e,ew) = rend3 bez
    sx = double2Float $ (s^.x)*sw
    sy = double2Float $ (s^.y)*sw
    cx = double2Float $ (c^.x)*cw
    cy = double2Float $ (c^.y)*cw
    dx = double2Float $ (d^.x)*dw
    dy = double2Float $ (d^.y)*dw
    ex = double2Float $ (e^.x)*ew
    ey = double2Float $ (e^.y)*ew
    swf = double2Float sw
    cwf = double2Float cw
    dwf = double2Float dw
    ewf = double2Float ew
    bpx = double2Float $ bp^.x
    bpy = double2Float $ bp^.y
  in
    V.fromList
      [ sx, sy, swf
      , cx, cy, cwf
      , dx, dy, dwf
      , ex, ey, ewf
      , bpx, bpy, 0
      ]
wholeSegToVertices bp seg@(WPathEArc _) = wholeSegToVertices bp
  (WPathSeg (makeSegment (evaluate seg 0) (evaluate seg 1)))

contourToVertices :: Contour -> V.Vector Float
contourToVertices cont = 
  V.concat 
    . fmap (wholeSegToVertices (getVertexC 0 cont)) 
    . BV.toList
    $ toWholeSegsC cont

convertToVertices :: ClosedPath -> V.Vector Float
convertToVertices = V.concat . fmap contourToVertices



testPath :: ClosedPath
testPath = glyphQ


vertices :: V.Vector Float
vertices = convertToVertices testPath
{-
vertices = V.fromList 
  -- bez1
  [  0.0,  0.8, 1
  ,  0.0,  0.8, 1
  , -0.8, -0.8, 1
  , -0.8, -0.8, 1
  -- bez2
  , -0.8, -0.8, 1
  , -0.4,  0.0, 1
  ,  0.4, -1.2, 1
  ,  0.8, -0.8, 1
  -- bez3
  ,  0.8, -0.8, 1
  ,  0.8, -0.8, 1
  ,  0.0,  0.8, 1
  ,  0.0,  0.8, 1
  ]
-}

{-
colors :: V.Vector Float
colors = V.fromList
  [
  ]
-}

