--
-- Shaders.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module GL.Util.Shaders
  ( module GL.Util.Shaders
  ) where

-- from https://github.com/bergey/haskell-OpenGL-examples/blob/master/src/Util/Shaders.hs

import Graphics.Rendering.OpenGL
import Control.Monad
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

{-
printError :: IO ()
printError = do 
  errs <- get errors 
  mapM_ (hPutStrLn stderr . ("GL: "++) . show)

makeShader :: ShaderType -> ByteString -> IO Shader
makeShader ty src = do
  s <- createShader ty
  shaderSourceBS s $= src
  compileShader s
  sOk <- get $ compileStatus s
  unless sOk $ do
    slog <- get $ shaderInfoLog s
    putStrLn $ "SLog: " ++ slog
    exitFailure
  printError
  return s

makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
  p <- createProgram
  mapM_ (attachShader p) shaders
  mapM_ (\(name,loc) -> attribLocation p name $= loc) attributes
  linkProgram p
  pOk <- get $ linkStatus p
  validateProgram p
  status <- get $ validateStatus p
  unless (pOk && status) $ do
    plog <- get $ programInfoLog p
    putStrLn $ "PLog: " ++ plog
    printError
    exitFailure
  return p

bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
  bindBuffer ArrayBuffer $= Just vbo
  vertexAttribPointer loc $= (ToFloat, dsc)
  vertexAttribArray loc $= Enabled

-}


