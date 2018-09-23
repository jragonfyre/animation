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
import Graphics.Text.TrueType.Internal

import Data.Vector (Vector, toList, (!))
import qualified Data.Vector.Unboxed as V

import Control.Lens ((^.))

-- module to hookup FontyFruity to my data types
-- might need to edit fonty fruity and whatnot to be better, but oh well.

data Glyph = Glyph
  { glyphChar :: Char
  , glyphLSBearing :: Double
  , glyphAdvance :: Double
  , glyphBaseline :: Double
  , glyphAscent :: Double
  , glyphDescent :: Double
  , glyphBBox :: Box
  , glyphPath :: ClosedPath
  }
  deriving (Show, Read, Eq, Ord)

{-
data Glyph =
  Glyph 
    { glyph :: ClosedPath -- in local coordinates
    , baseline :: Double -- baseline in local coordinates
    , leftsideBearing :: Double
    , advance :: Double -- width in local coordinates
    , ascent :: Double -- height of character above baseline
    , descent :: Double -- distance character goes below baseline
    , glyphBBox :: Box
    }
  deriving (Show, Read, Eq, Ord)
-}

instance GBounded Glyph where
  bounds = bounds . glyphPath

instance Translatable Glyph where
  translate vec glyf =
    let
      bbox = glyphBBox glyf
      bl = glyphBaseline glyf
      gp = glyphPath glyf
    in
      glyf{glyphBBox = translate vec bbox, glyphBaseline = bl + (vec^.y), glyphPath = fmap (translate vec) gp}
instance Scalable Glyph where
  scale s glyf@Glyph{..} =
    Glyph
      glyphChar
      (s*glyphLSBearing)
      (s*glyphAdvance)
      (s*glyphBaseline)
      (s*glyphAscent)
      (s*glyphDescent)
      (scale s glyphBBox)
      (fmap (scale s) glyphPath)


makeGlyph :: Font -> Char -> Glyph
makeGlyph font char = 
  let
    metrix = _fontHorizontalMetrics font
    (adv,rg) = getCharacterGlyphsAndMetrics font char
    ix = _rawGlyphIndex (rg ! 0)
    gPath = (concat . map rawGlyphToPath . toList $ rg)
    bbox = bounds gPath
    bearing = maybe 0 (\mtrx -> fromIntegral . _hmtxLeftSideBearing $ (_glyphMetrics mtrx)!ix) metrix
  in
    Glyph char bearing (float2Double adv) 0 (boxTop bbox) (boxBottom bbox) bbox gPath

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




