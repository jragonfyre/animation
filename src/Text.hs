--
-- Text.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Text
  ( module Text
  ) where

import Geometry.Types
import Geometry.Path
import Geometry.Affine
import Control.Lens ((^.))

import Graphics.Text.TrueType
import Graphics.Text.TrueType.Internal

import Data.List (foldl', unfoldr, mapAccumL)
import Data.Int

import Font

import qualified Data.Maybe as M

instance Baseline Glyph where
  baseline = glyphBaseline

newtype GlyphString = GlyphStr (BasedBox, ClosedPath)
  deriving (Show,Eq,Ord,Read)

instance GBounded GlyphString where
  bounds (GlyphStr (bbox,_)) = bounds bbox
  
instance Baseline GlyphString where
  baseline (GlyphStr (bbox,_)) = baseline bbox

instance Translatable GlyphString where
  translate vec (GlyphStr (bb,cp)) = GlyphStr (translate vec bb,fmap (translate vec) cp)
instance Scalable GlyphString where
  scale s (GlyphStr (bb,cp)) = GlyphStr (scale s bb,fmap (scale s) cp)

glyphStringContours :: GlyphString -> ClosedPath
glyphStringContours (GlyphStr (_,conts)) = conts

fwordToDouble :: FWord -> Double
fwordToDouble (FWord fw) = fromIntegral ((fromIntegral fw):: Int16) :: Double

fwordToInt16 :: FWord -> Int16
fwordToInt16 (FWord fw) = (fromIntegral fw) :: Int16


-- takes a word with no white space
buildGlyphString :: Font -> Double -> String -> GlyphString
buildGlyphString font lineSize str = 
  let
    hhea = M.fromJust $ _fontHorizontalHeader font
    hheaLGI = fwordToInt16 $ _hheaLineGap hhea
    mascI = fwordToInt16 $ _hheaAscent hhea
    masc = fromIntegral mascI
    mdescI = fwordToInt16 $ _hheaDescent hhea
    mdesc = fromIntegral mdescI
    hheaLG = if hheaLGI == 0 then 1.1*(masc-mdesc) else fromIntegral hheaLGI
    scaling = lineSize/hheaLG
    sAsc = masc * scaling
    sDesc = mdesc * scaling
    mCharSize = sAsc - sDesc
    balancing = (lineSize - mCharSize)/2
    (endPos,conts) = 
      foldl
        ( \(penpos,conts) char -> 
            let
              glyf = makeGlyph font char
              glyph = scale scaling glyf
              tglyph =
                translate
                  ( makeVector
                      (penpos + (glyphLSBearing glyph) - (boxLeft (glyphBBox glyph)))
                      (-(glyphBaseline glyph))
                  )
                  glyph
            in
              (penpos + (glyphAdvance tglyph), (glyphPath tglyph) ++ conts)
        )
        (0,[])
        str
  in
    GlyphStr (BasedBox (makeBoxSides 0 endPos (sAsc + balancing) (sDesc-balancing), 0), conts)


data Justification 
  = Justify Double Double Double
  | RaggedRight Double
  | RaggedLeft Double
  | Center Double
  deriving (Show, Eq, Ord, Read)

idealSpacing :: Justification -> Double
idealSpacing (Justify _ is _) = is
idealSpacing (RaggedRight is) = is
idealSpacing (RaggedLeft is) = is
idealSpacing (Center is) = is

data LayoutStyle =
  LayoutStyle
    { lineWidth :: Double
    , lineHeight :: Double
    , justification :: Justification
    }
  deriving (Show, Eq, Ord, Read)


class Baseline a where
  baseline :: a -> Double -- ^ marks the baseline of an object

basedBounds :: (Baseline a, GBounded a) => a -> BasedBox
basedBounds val = BasedBox (bounds val, baseline val)

newtype BasedBox = BasedBox (Box,Double)
  deriving (Show, Eq, Ord, Read)

newtype MonospaceString = MonoStr String
  deriving (Show, Eq, Ord, Read)

instance Baseline MonospaceString where
  baseline _ = 0

instance GBounded MonospaceString where
  bounds (MonoStr str) = makeBoxSides 0 (fromIntegral $ length str) (-0.4) (1.6)

monospaceLayout :: Justification -> LayoutStyle
monospaceLayout = LayoutStyle 80 2

justifyMonospace :: LayoutStyle
justifyMonospace = monospaceLayout (Justify 0.8 1 1.5)

emptyMonoLB :: LineBuilder MonospaceString
emptyMonoLB = emptyLineBuilder justifyMonospace

instance Baseline BasedBox where
  baseline (BasedBox (_,bl)) = bl

instance GBounded BasedBox where
  bounds (BasedBox (bx,_)) = bx

instance Translatable BasedBox where
  translate v (BasedBox (bx,bl)) = BasedBox (translate v bx, bl + (v^.y))
instance Scalable BasedBox where
  scale s (BasedBox (bx,bl)) = BasedBox (scale s bx, s*bl)

data Fullness
  = Full
  | Linebreak 
  | Overfull
  deriving (Show, Eq, Ord, Read)
  -- -| Underfull

ascent :: (GBounded a, Baseline a) => a -> Double
ascent val = (boxTop $ bounds val) - (baseline val)

descent :: (GBounded a, Baseline a) => a -> Double
descent val = (boxBottom $ bounds val) - (baseline val)

data LineBuilder a = LineBuilder
  { curItemsRev :: [a]
  , curLineLength :: Double
  , numItems :: Int
  , isFull :: Bool
  , isOverfull :: Bool
  , lbLayoutStyle :: LayoutStyle
  }
  deriving (Show, Eq, Ord, Read)

emptyLineBuilder :: LayoutStyle -> LineBuilder a
emptyLineBuilder = LineBuilder [] 0 0 False False

appendItem :: (GBounded a) => LineBuilder a -> a -> (LineBuilder a, Maybe a)
appendItem lb@(LineBuilder cis cll ni iF iOF ls) item = 
  if iF
  then
    (lb, Just item)
  else
    case justification ls of
      Justify minSp idealSp maxSp ->
        let
          bx = bounds item
          bxLen = boxLength bx
          lw = lineWidth ls
        in
          if (cll+bxLen+(fromIntegral ni)*minSp) > lw
          then
            if lw-cll > maxSp * (fromIntegral $ ni-1) -- switch to a TeX style penalty system. 
              -- It'll work better xP Yikes this doesn't work well :/
            then
              (LineBuilder (item:cis) (cll+bxLen) (ni+1) True True ls, Nothing)
            else
              (LineBuilder cis cll ni True False ls, Just item)
          else
            (LineBuilder (item:cis) (cll+bxLen) (ni+1) False False ls, Nothing)
      just -> 
        let
          sp = idealSpacing just
          bx = bounds item
          bxLen = boxLength bx
          lw = lineWidth ls
        in
          if (cll+bxLen+(fromIntegral ni)*sp) > lw
          then
            (LineBuilder cis cll ni True False ls, Just item)
          else
            (LineBuilder (item:cis) (cll+bxLen) (ni+1) False False ls, Nothing)

buildLine :: (GBounded a, Baseline a) => LineBuilder a -> LineText a
buildLine (LineBuilder cis cll ni iF iOF ls) =
  let
    (its,asc,desc) = 
      foldl'
        ( \(its,casc,cdesc) item -> 
            ( item:its
            , max casc (ascent item)
            , min cdesc (descent item)
            )
        )
        ([],0,0)
        cis
  in
    LineText
      ( if iF
        then
          if iOF
          then
            Overfull
          else
            Full
        else
          Full -- TODO FIXXX!!
      )
      ls
      its
      asc
      desc
      cll
      (max (ni-1) 0)

data LineText a = LineText 
  { lineFullness :: Fullness
  , lineLayoutStyle :: LayoutStyle
  , lineItems :: [a]
  , lineAscent :: Double
  , lineDescent :: Double
  , totalItemWidth :: Double
  , numSpaces :: Int
  }
  deriving (Show, Eq, Ord, Read)

--buildLineText 

spaceWidth :: LineText a -> Double
spaceWidth lt = 
  let
    ls = lineLayoutStyle lt
  in case justification ls of
    Center sp ->
      sp
    RaggedRight sp ->
      sp
    RaggedLeft sp ->
      sp
    Justify minSp idealSp maxSp ->
      let
        lw = lineWidth ls
        iw = totalItemWidth lt
        ns = numSpaces lt
      in
        case lineFullness lt of
          Full -> (lw-iw)/(fromIntegral ns)
          Linebreak -> idealSp
          Overfull -> minSp

formLine :: (GBounded a, Baseline a) => LayoutStyle -> [a] -> Maybe (LineText a,[a])
formLine _ [] = Nothing -- there is no line to begin
formLine ls items = Just $ formLineHelper (emptyLineBuilder ls) ls items  -- there is at least some line
  where
    formLineHelper lb ls [] = (buildLine lb, []) -- assumes there is at least some line
    formLineHelper lb ls (x:xs) = 
      let
        (newLb,mVal) = appendItem lb x
      in
        case mVal of
          Just x ->
            (buildLine newLb, x:xs)
          Nothing ->
            formLineHelper newLb ls xs

formLines :: (GBounded a, Baseline a) => LayoutStyle -> [a] -> [LineText a]
formLines ls = reverse . unfoldr (formLine ls)

basedOrigin :: (GBounded a, Baseline a) => a -> Point
basedOrigin val = makePoint (boxLeft (bounds val)) (baseline val)

renderLine :: (Translatable a, GBounded a, Baseline a) => LayoutStyle -> Point -> LineText a -> [a]
renderLine ls st lt = 
  let
    lw = lineWidth ls
    iw = totalItemWidth lt
    ns = numSpaces lt
    spWidth = case justification ls of
      Justify minSp idealSp maxSp ->
        case lineFullness lt of 
          Overfull -> minSp
          Linebreak -> idealSp
          Full -> (lw - iw)/(fromIntegral ns)
      Center sp -> sp
      RaggedRight sp -> sp
      RaggedLeft sp -> sp
    (_,titems) = mapAccumL 
      ( \curDisp item ->
          ( curDisp + spWidth + boxLength (bounds item)
          , translate 
              ((makeVector curDisp 0) +. (st -. (basedOrigin item)))
              item
          )
      )
      0
      (lineItems lt)
  in
    titems
    

layout :: (GBounded a, Translatable a, Baseline a) => LayoutStyle -> Point -> [a] -> [a]
layout ls st its = 
  let 
    lineFeedVec = makeVector 0 (lineHeight ls)
  in
    concat 
      $ zipWith 
          (\i -> renderLine ls (st+.(((fromIntegral i)::Double)*.lineFeedVec)))
          [0..]
          (formLines ls its)



