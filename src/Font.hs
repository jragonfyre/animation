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
import Geometry.Path
import Geometry.PathBuilder
import GHC.Float (float2Double)
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



