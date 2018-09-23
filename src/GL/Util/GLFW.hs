--
-- GLFW.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module GL.Util.GLFW
  ( module GL.Util.GLFW
  ) where

-- from https://github.com/bergey/haskell-OpenGL-examples/blob/master/src/Util/GLFW.hs 

import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import System.Exit
import System.IO
import System.Clock
import GHC.Float

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
  GLFW.setWindowShouldClose window True

initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  if not successfulInit
  then exitFailure
  else
    do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DoubleBuffer True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
      GLFW.windowHint $ GLFW.WindowHint'Samples (Just 8)
      mw <- GLFW.createWindow 640 480 title Nothing Nothing
      case mw of 
        Nothing -> GLFW.terminate >> exitFailure
        Just window ->
          do
            GLFW.makeContextCurrent mw
            GLFW.setKeyCallback window (Just keyCallback)
            return window

cleanup :: GLFW.Window -> IO ()
cleanup win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

getTimeFloat :: IO Float
getTimeFloat = do
  time <- getTime Realtime
  let 
    sDouble = fromIntegral $ (sec time) `mod` 3600
    nsDouble = fromIntegral $ nsec time
    dtime = sDouble + (10**(-9))*nsDouble
  return $ double2Float dtime


mainLoop :: (Float -> IO ()) -> GLFW.Window -> IO ()
mainLoop draw w = do
  close <- GLFW.windowShouldClose w
  unless close $ do
    t <- getTimeFloat
    --putStrLn $ show t
    draw t
    GLFW.swapBuffers w
    GLFW.pollEvents
    mainLoop draw w

