--
-- Font.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Font
  ( module Font
  , Font
  ) where

import Geometry.Types
import Geometry.Affine
import Geometry.Path
import Geometry.PathBuilder
import GHC.Float (float2Double,double2Float)
import Graphics.Text.TrueType

import Data.Vector (Vector, toList)
import qualified Data.Vector.Unboxed as V

-- module to hookup FontyFruity to my data types
-- might need to edit fonty fruity and whatnot to be better, but oh well.

data Glyph = Glyph
  { glyphChar :: Char
  , glyphAdvance :: Double
  , glyphPath :: ClosedPath
  }
  deriving (Show, Read, Eq, Ord)

instance GBounded Glyph where
  bounds = bounds . glyphPath

makeGlyph :: Font -> Char -> Glyph
makeGlyph font char = 
  let
    (adv,rg) = getCharacterGlyphsAndMetrics font char
  in
    Glyph char (float2Double adv) (concat . map rawGlyphToPath . toList $ rg)

rawGlyphToPath :: RawGlyph -> ClosedPath
rawGlyphToPath = process . concat . map (rgListToSCP . V.toList) . _rawGlyphContour
  where 
    rgListToSCP [] = []
    rgListToSCP ((x,y):xs) = (MA (fromIntegral x) (fromIntegral y)):(rgLHelper xs)
    rgLHelper [] = [Z]
    rgLHelper [x] = [Z]
    rgLHelper ((x1,y1):(x2,y2):xs) =
      (QA (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2)) : (rgLHelper xs)


stringToPath :: Int -> Double -> Font -> String -> ClosedPath
stringToPath dpi pts ft str = 
  let
    contours = concat $
      getStringCurveAtPoint
        (dpi)
        (0,0)
        [(ft, PointSize (double2Float pts), str)]
    contourToSCP [] = []
    contourToSCP ((x,y):xs) = (MA (float2Double x) (float2Double y)):(contHelper xs)
    contHelper [] = [Z]
    contHelper [x] = [Z]
    contHelper ((x1,y1):(x2,y2):xs) =
      (QA (float2Double x1) (float2Double y1) (float2Double x2) (float2Double y2)) : (contHelper xs)
  in
    transform (matrixToAffine (diagonal 1 (-1))) . process . concat . map (contourToSCP . V.toList) $ contours




