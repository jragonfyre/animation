--
-- Rasterizer.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--


module Rasterizer
  ( module Rasterizer
  ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa (DIM2, Array, (:.) (..), Z(..), fromFunction, ix2, (!), ix1, Source, Structured (..))
--import Data.Array.Repa.Repr.Vector as R

import qualified Data.Vector as V

import Data.List (zipWith4)
import qualified Data.List as L

import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

import Geometry
import Geometry.PathBuilder (glyphQ)

--import Geometry.Region

import Scanner

import Model
import Picture hiding (orange)

import Utils

import Control.Lens ((^.),from) --,(%~),_1,(&),to)

--import Control.DeepSeq (deepseq)

import qualified Graphics.Image as I
import Graphics.Image (RPU,RGB,Image)
import Graphics.Image.Interface.Repa (fromRepaArrayP)

import Control.Monad.IO.Class


type Raster r a = Array r DIM2 a

rasterizeBoundary :: (Structured t Double Double) => Rasterizer a t -> Rasterizer a (TR t)
rasterizeBoundary rasterizer = \sz box a -> R.smap (\t -> 4*t*(1-t)) $ rasterizer sz box a

--  this is the normal pdf, not sure why this is here.
--norm t = exp (-t^2)

{-
plusDistribution :: V.Vector Double
plusDistribution = V.fromList $ map ((50*) . norm . (/5)) [0..10]

plusWeight = 4*(V.sum plusDistribution) - 3*((V.!) plusDistribution 0)

plusStencil = makeStencil2 21 21 
  $ \(Z:.i:.j) -> 
      if i == 0
      then
        Just ((V.!) plusDistribution (abs j))
      else
        if j == 0
        then
          Just ((V.!) plusDistribution (abs i))
        else
          Nothing

plusAntialias :: (Source t Double) => Raster t Double -> Raster R.D Double 
plusAntialias raster = 
  let
    smoothed = 
      mapStencil2
        BoundClamp
        plusStencil
        raster
  in 
    R.zipWith (\o n -> if 0 < o && o < 1 then n/273 else o) raster smoothed
-}

-- the Gaussian below has total weight 273
gaussian1 = 
  [stencil2| 1  4  7  4 1 
             4 16 26 16 4
             7 26 41 26 7
             4 16 26 16 4
             1  4  7  4 1 |]
gaussian1Weight = 273

gaussian2 =
  [stencil2| 0  0  1   2  1  0 0
             0  3 13  22 13  3 0
             1 13 59  97 59 13 1
             2 22 97 159 97 22 2
             1 13 59  97 59 13 1
             0  3 13  22 13  3 0
             0  0  1   2  1  0 0 |]
gaussian2Weight = 1003

stencilAntialias :: (Source t Double) => Stencil DIM2 Double -> Double -> Raster t Double -> Raster R.D Double
stencilAntialias stencil weight raster = 
  let
    smoothed = 
      mapStencil2
        BoundClamp
        stencil
        raster
  in 
    R.zipWith (\o n -> if 0 < o && o < 1 then n/weight else o) raster smoothed

stencilAABlur :: (Source t Double) => Stencil DIM2 Double -> Double -> Raster t Double -> Raster R.D Double
stencilAABlur stencil weight raster = R.delay . smap (/weight) $ mapStencil2 BoundClamp stencil raster

gaussian1Antialias :: (Source t Double) => Raster t Double -> Raster R.D Double
gaussian1Antialias = stencilAntialias gaussian1 gaussian1Weight

gaussian2Antialias :: (Source t Double) => Raster t Double -> Raster R.D Double
gaussian2Antialias = stencilAntialias gaussian2 gaussian2Weight

--gaussianFilter :: Int -> Double -> Array R.D DIM2 

signSqrt :: Double -> Double
signSqrt x | x >= 0 = sqrt x
           | otherwise = -(sqrt (-x))

rasterizeCircle :: (Int, Int) -> Box -> Circle -> Raster R.D Double 
rasterizeCircle (nx,ny) box circ = 
  let 
    (center,radius) = circ^.circAsPair
    (cx,cy) = center^.ptAsPair
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornxLoc = \(Z:.i) -> boxLeft box + fromIntegral i * pixWidth
    ylocInv = \h -> (boxTop box - h)/pixHeight
    lower = fromFunction (ix1 (nx+1)) $ \ix -> ylocInv $ cy+(signSqrt $ radius^2-(cornxLoc ix-cx)^2)
    upper = fromFunction (ix1 (nx+1)) $ \ix -> ylocInv $ cy-(signSqrt $ radius^2-(cornxLoc ix-cx)^2)
    corners =
      fromFunction (ix2 (nx+1) (ny+1)) 
        $ \(Z:.i:.j) ->
            let
              yl = fromIntegral j
              ix = ix1 i
            in
              if (yl < upper!ix) && (yl > lower!ix)
              then
                0.25
              else
                0
    vals =
      mapStencil2 
        BoundClamp
        [stencil2| 0 1 1
                   0 1 1
                   0 0 0 |]
        corners
  in
    R.extract (ix2 0 0) (ix2 nx ny) vals


{-
rasterizeConvex :: (Int, Int) -> Box -> ConvexPolytope -> Raster R.D Double
rasterizeConvex (nx,ny) box cp =
  let
    (width,height) = box^.dimensions.vecAsPair
    (lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornIxLoc 
      = \(Z:.i:.j) -> 
          (boxLeft box + fromIntegral i*pixWidth,boxTop box - fromIntegral j*pixHeight)^.from ptAsPair
    corners =
      fromFunction (ix2 (nx+1) (ny+1)) 
        $ (region^.inside) . cornIxLoc
    computeIntensity ((Z:.i):.j) = 
      let
        testIxs = [ix2 i j,ix2 (i+1) j,ix2 i (j+1),ix2 (i+1) (j+1)]
        inCorners = length $ filter (corners!) testIxs
      in
        if inCorners == 0
        then
          0
        else
          if inCorners == 4
          then
            1
          else
            fromIntegral inCorners/4
  in
    fromFunction (ix2 nx ny) computeIntensity
-}

sndOfThree :: (a,b,c) -> b
sndOfThree (_,x,_) = x

scanRasterBezierTolerance = 1e-8

scanRasterDuplicateTolerance :: Double -> Double
scanRasterDuplicateTolerance pixWidth = 3*pixWidth

-- assumes sorted on the (first) double
keepFirst :: V.Vector (Int,Int) -> Double -> [(Int, (Double,(Sign,Double)))] -> [(Int, (Double, (Sign,Double)))]
keepFirst _ _ [] = []
keepFirst _ _ [x] = [x]
keepFirst ps tol ((t1@(i,(x1,(s1,_)))):(t2@(j,(x2,(s2,_)))):xs) =
  -- improve detection of whether i and j are adjacent segments or not
  let 
    (pi,si) = (V.!) ps i
  in 
    if (j == pi || j == si) && s1 == s2 && (abs (x1-x2) < tol)
    then
      t1:(keepFirst ps tol xs)
    else
      t1:(keepFirst ps tol (t2:xs))

scanRasterizerNoAA :: (Int,Int) -> Box -> ClosedPath -> IO (Raster R.D Double)
scanRasterizerNoAA (nx,ny) box cp =
  let 
    bt=boxTop box
    segsPS = toWholeSegsPredSucc cp
    segs = V.map fst segsPS
    ps = V.map snd segsPS
    segixs = V.enumFromN 0 (V.length segs)
    --ssegs = V.fromList $ L.sortOn (negate . boxTop . wpSegBoundingBox) segs
    --ssegixs satisfies V.map ((V.!) segs) ssegixs == ssegs
    --invssegixs satisfies V.map ((V.!) ssegixs) invssegixs = V.enumFrom N 0 (V.length segs)
    --and vice versa
    ssegixs = V.fromList $ L.sortOn (negate . boxTop . wpSegBoundingBox . (V.!) segs) $ V.toList segixs
    --invssegixs = V.map fst V.fromList $ L.sortOn snd . V.toList V.zip segixs ssegixs
    --ssegixs = V.enumFromN 0 (V.length ssegs)
    segstopy = fmap (boxTop . wpSegBoundingBox) segs
    --segstopy = fmap (boxTop . wpSegBoundingBox) ssegs
    segsbottomy = fmap (boxBottom . wpSegBoundingBox) segs
    --segsbottomy = fmap (boxBottom . wpSegBoundingBox) ssegs
    (width,height) = box^.dimensions.vecAsPair
    --(lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    centxLoc = \i -> boxLeft box + (0.5+fromIntegral i) * pixWidth
    centyLoc = \j -> bt - (0.5+fromIntegral j)*pixHeight
    --ylocInv = \h -> (bt - h)/pixHeight
    relevantSegsUF = \(j,prevCursegs,prevRemsegs) -> 
      let
        yv = centyLoc j
        (newcsegs,newRemsegs) = V.span (\i -> yv <= (V.!) segstopy i) prevRemsegs
        newCursegs = V.filter (\i -> yv >= (V.!) segsbottomy i) ((V.++) newcsegs prevCursegs)
      in
        if j > ny
        then
          Nothing
        else
          Just (newCursegs, (j+1,newCursegs,newRemsegs))
    relSegs = V.unfoldrN (ny+1) relevantSegsUF (0,V.empty,ssegixs)
    critPts = 
      V.imap
        (\j sgixs -> 
          L.map snd
            . keepFirst ps (scanRasterDuplicateTolerance pixWidth)
            . L.sortOn (fst . snd)
            . L.concat 
            . V.toList 
            $ fmap
                (\i -> 
                  fmap (i,)
                    $ solveWPSegNTF scanRasterBezierTolerance (centyLoc j) 
                    --  $ (V.!) ssegs i
                    $ (V.!) segs i
                )
                sgixs
        )
        relSegs
    unfoldRow = \(i, cursum, remcrits) -> 
      case remcrits of
        [] ->
          Just (indicate (cursum /= 0),(i+1,cursum,[]))
        _ ->
          let
            xv = centxLoc i
            (ncrits, rcrits) = L.span (\c -> (fst c) < xv-pixWidth) remcrits
            ccrits = L.takeWhile (\c -> (fst c) < xv +pixWidth) rcrits
            newsum = cursum + (sum $ map (signValue . fst . snd) (ncrits))
            tsum = newsum + (sum $ map (signValue . fst . snd) (L.takeWhile (\c -> (fst c) < xv) ccrits))
            d = case ccrits of 
              [] ->
                1
              ((x,(_,xscale)):_) -> 
                1/(1+(exp (-4*(xv-x)*xscale/pixWidth)))
                --1/(1+(exp (-7*(xv-x)*xscale/pixWidth)))
                --0.5*(exp (-10*((xv-x)*xscale/(pixWidth))^2))
          in
            -- TODO: Fix this expression
            --Just (if (tsum == 0) then 0+d else 1-d, (i+1,newsum,rcrits))
            Just (d*(indicate (tsum/=0)) + (1-d)*(indicate (newsum /= 0)), (i+1,newsum,rcrits))
    rows = V.map (\cs -> V.unfoldrN (nx+1) unfoldRow (0,0,cs))  critPts
    vals =
      fromFunction (ix2 nx ny) 
        $ \(Z:.i:.j) ->
            ((V.!) ((V.!) rows j) i)
  in
    do
      {-
      --debugging code
      putStrLn "segs"
      --putStrLn $ show segs
      putStrLn "ssegs"
      --putStrLn $ show ssegs
      putStrLn "ssegixs"
      --putStrLn $ show ssegixs
      putStrLn "segstopy"
      --putStrLn $ show segstopy
      putStrLn "segsbottomy"
      --putStrLn $ show segsbottomy
      putStrLn "relSegsUF"
      --putStrLn $ show $ relevantSegsUF (0,V.empty,ssegixs)
      putStrLn "relSegs"
      --putStrLn $ show relSegs
      putStrLn "critPts"
      --putStrLn $ show critPts
      putStrLn "odd crit pts"
      putStrLn $ show $ V.filter (odd . length) critPts
      putStrLn "rows"
      --putStrLn $ show rows
      --return $ R.extract (ix2 0 0) (ix2 nx ny) vals
      -}
      return vals

-- need to switch to accelerate to get a proper speedup :/
-- maybe?
scanRasterizer :: (Int,Int) -> Box -> ClosedPath -> IO (Raster R.D Double)
scanRasterizer (nx,ny) box cp =
  let 
    bt=boxTop box
    segsPS = toWholeSegsPredSucc cp
    segs = V.map fst segsPS
    ps = V.map snd segsPS
    segixs = V.enumFromN 0 (V.length segs)
    --ssegs = V.fromList $ L.sortOn (negate . boxTop . wpSegBoundingBox) segs
    --ssegixs satisfies V.map ((V.!) segs) ssegixs == ssegs
    --invssegixs satisfies V.map ((V.!) ssegixs) invssegixs = V.enumFrom N 0 (V.length segs)
    --and vice versa
    ssegixs = V.fromList $ L.sortOn (negate . boxTop . wpSegBoundingBox . (V.!) segs) $ V.toList segixs
    --invssegixs = V.map fst V.fromList $ L.sortOn snd . V.toList V.zip segixs ssegixs
    --ssegixs = V.enumFromN 0 (V.length ssegs)
    segstopy = fmap (boxTop . wpSegBoundingBox) segs
    --segstopy = fmap (boxTop . wpSegBoundingBox) ssegs
    segsbottomy = fmap (boxBottom . wpSegBoundingBox) segs
    --segsbottomy = fmap (boxBottom . wpSegBoundingBox) ssegs
    (width,height) = box^.dimensions.vecAsPair
    --(lx,ly) = box^.corner.ptAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    cornxLoc = \i -> boxLeft box + fromIntegral i * pixWidth
    cornyLoc = \j -> bt - fromIntegral j*pixHeight
    --ylocInv = \h -> (bt - h)/pixHeight
    relevantSegsUF = \(j,prevCursegs,prevRemsegs) -> 
      let
        yv = cornyLoc j
        (newcsegs,newRemsegs) = V.span (\i -> yv <= (V.!) segstopy i) prevRemsegs
        newCursegs = V.filter (\i -> yv >= (V.!) segsbottomy i) ((V.++) newcsegs prevCursegs)
      in
        if j > ny
        then
          Nothing
        else
          Just (newCursegs, (j+1,newCursegs,newRemsegs))
    relSegs = V.unfoldrN (ny+1) relevantSegsUF (0,V.empty,ssegixs)
    critPts = 
      V.imap
        (\j sgixs -> 
          L.map snd
            . keepFirst ps (scanRasterDuplicateTolerance pixWidth)
            . L.sortOn (fst . snd)
            . L.concat 
            . V.toList 
            $ fmap
                (\i -> 
                  fmap (i,)
                    $ solveWPSegNTF scanRasterBezierTolerance (cornyLoc j) 
                    --  $ (V.!) ssegs i
                    $ (V.!) segs i
                )
                sgixs
        )
        relSegs
    unfoldRow = \(i, cursum, remcrits) -> 
      case remcrits of
        [] ->
          Just (indicate (cursum /= 0),(i+1,cursum,[]))
        _ ->
          let
            xv = cornxLoc i
            (ncrits, rcrits) = L.span (\c -> (fst c) < xv-pixWidth) remcrits
            ccrits = L.takeWhile (\c -> (fst c) < xv +pixWidth) rcrits
            newsum = cursum + (sum $ map (signValue . fst . snd) (ncrits))
            tsum = newsum + (sum $ map (signValue . fst . snd) (L.takeWhile (\c -> (fst c) < xv) ccrits))
            d = case ccrits of 
              [] ->
                1
              ((x,(_,xscale)):_) -> 
                1/(1+(exp (-7*(xv-x)*xscale/pixWidth)))
                --0.5*(exp (-10*((xv-x)*xscale/(pixWidth))^2))
          in
            -- TODO: Fix this expression
            --Just (if (tsum == 0) then 0+d else 1-d, (i+1,newsum,rcrits))
            Just (d*(indicate (tsum/=0)) + (1-d)*(indicate (newsum /= 0)), (i+1,newsum,rcrits))
    rows = V.map (\cs -> V.unfoldrN (nx+1) unfoldRow (0,0,cs))  critPts
    corners =
      fromFunction (ix2 (nx+1) (ny+1)) 
        $ \(Z:.i:.j) ->
            0.25 * ((V.!) ((V.!) rows j) i)
    --corners :: Array V DIM2 Double
    --corners = R.computeS $ R.map indicate $ R.fromVector (ix2 (nx+1) (ny+1)) $ V.concat rows
    vals =
      mapStencil2 
        BoundClamp
        [stencil2| 0 1 1
                   0 1 1
                   0 0 0 |]
        corners
  in
    do
      {-
      --debugging code
      putStrLn "segs"
      --putStrLn $ show segs
      putStrLn "ssegs"
      --putStrLn $ show ssegs
      putStrLn "ssegixs"
      --putStrLn $ show ssegixs
      putStrLn "segstopy"
      --putStrLn $ show segstopy
      putStrLn "segsbottomy"
      --putStrLn $ show segsbottomy
      putStrLn "relSegsUF"
      --putStrLn $ show $ relevantSegsUF (0,V.empty,ssegixs)
      putStrLn "relSegs"
      --putStrLn $ show relSegs
      putStrLn "critPts"
      --putStrLn $ show critPts
      putStrLn "odd crit pts"
      putStrLn $ show $ V.filter (odd . length) critPts
      putStrLn "rows"
      --putStrLn $ show rows
      -}
      return $ R.extract (ix2 0 0) (ix2 nx ny) vals
      --return $ stencilAntialias gaussian1 gaussian1Weight $ R.extract (ix2 0 0) (ix2 nx ny) vals


-- screenx increases left to right
-- screeny increases top to bottom
-- 0,0 is the upper left hand corner of the upper left hand corner pixel
-- maxx,maxy is the lower right hand corner of the lower right hand corner pixel
-- i.e. there are maxx-1 and maxy-1 columns and rows of pixels respectively
screenToAbstract :: (Real a) => (a,a) -> Box -> (a,a) -> (Double,Double)
screenToAbstract (maxx,maxy) abstractBox (screenx,screeny) = 
  let
    zoX = (realToFrac screenx)/(realToFrac maxx) :: Double
    zoY = (realToFrac screeny)/(realToFrac maxy) :: Double
    (width,height) = abstractBox^.dimensions.vecAsPair
  in
    (boxLeft abstractBox + zoX*width, boxTop abstractBox - zoY*height)


pixelToAbstract :: (Real a) => (a,a) -> Box -> (Int,Int) -> (a,a) -> (Double,Double)
pixelToAbstract mx absBnds (pixx,pixy) (subpixx,subpixy) = 
  screenToAbstract mx absBnds (fromIntegral pixx + subpixx,fromIntegral pixy+subpixy)

{-
abstractToScreen :: (Int,Int) -> Box -> (Double,Double) -> (Double,Double)
abstractToScreen (maxx,maxy) abstractBox (absx,absy) =
  let
    (width,height) = abstractBox^.dimensions.vecAsPair

  in
-}
    

-- computes smallest pixel aligned bounding box containing the given box (intersected with the bigBox)
-- returns the box and its displacement in pixels
pixelAlignedBoundingBox :: (Int,Int) -> Box -> Box -> (Box,(Int,Int),(Int,Int))
pixelAlignedBoundingBox (nx,ny) bigBox boundBox = 
  let
    bb = intersectionBoxes [bigBox,boundBox]
    (width,height) = bigBox^.dimensions.vecAsPair
    llc = bigBox^.corner
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    bllc = bb^.corner
    llcvec = bllc-.llc
    (lcdx,lcdy) = llcvec^.vecAsPair
    pdx = floor (lcdx/pixWidth)
    pdy = floor (lcdy/pixHeight)
    fcdx = pixWidth * (fromIntegral pdx)
    fcdy = pixHeight * (fromIntegral pdy)
    urcvec = llcvec +. (bb^.dimensions)
    (ucdx,ucdy) = urcvec^.vecAsPair
    pDx = ceiling (ucdx/pixWidth)
    pDy = ceiling (ucdy/pixHeight)
    ccdx = pixWidth * (fromIntegral pDx)
    ccdy = pixHeight * (fromIntegral pDy)
    (bsx,bsy) = llc^.ptAsPair
  in
    ( makeBox (makePoint (bsx+fcdx) (bsy+fcdy)) (makeVector (ccdx-fcdx) (ccdy-fcdy))
    , (pdx,ny-pDy)
    , (pDx-pdx,pDy-pdy)
    )

type Rasterizer r t = (Int,Int) -> Box -> r -> Raster t Double
type MRasterizer r t m = (Int,Int) -> Box -> r -> m (Raster t Double)

background :: (Int,Int) -> Raster R.D LRGBA
background (nx,ny) = fromFunction (ix2 nx ny) (const invisible)

type Compositor = LRGBA -> LRGBA -> LRGBA

render :: (Source r LRGBA, Source t Double) => Raster r LRGBA -> Compositor -> (Int,Int) -> Box -> a -> Fill -> Rasterizer a t -> Raster R.D LRGBA
render bg comp sz@(nx,ny) box reg fill rasterizer = 
  let
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    centIxLoc (Z:.i:.j) = 
      (boxLeft box + (fromIntegral i+0.5)*pixWidth,boxTop box - (fromIntegral j+0.5)*pixHeight)^.from ptAsPair
  in
    R.zipWith (flip comp) bg 
      $ R.traverse
          (rasterizer sz box reg)
          id
          (\lkup ix -> ((lkup ix)*.) . fill . centIxLoc $ ix)

mrender :: (Source r LRGBA, Source t Double, Monad m,GBounded a, MonadIO m) =>
  Raster r LRGBA -> Compositor -> (Int,Int) -> Box -> a -> Fill -> MRasterizer a t m -> m (Raster R.D LRGBA)
mrender bg comp sz@(nx,ny) box reg fill rasterizer = 
  let
    bbox = bounds reg
    (pabb,(spx,spy),(npx,npy)) = pixelAlignedBoundingBox sz box bbox
    lpx = spx+npx
    lpy = spy+npy
    (width,height) = box^.dimensions.vecAsPair
    pixWidth=width/fromIntegral nx
    pixHeight=height/fromIntegral ny
    centIxLoc (Z:.i:.j) = 
      (boxLeft box + (fromIntegral i+0.5)*pixWidth,boxTop box - (fromIntegral j+0.5)*pixHeight)^.from ptAsPair
  in do
    {-
    -- old slow version of code.
    rast <- rasterizer sz box reg
    return 
      $ R.zipWith (flip comp) bg 
      $ R.traverse
          rast
          id
          (\lkup ix -> ((lkup ix)*.) . fill . centIxLoc $ ix)
    -}
    {-
    -- yay this is faster! :D
    rast <- rasterizer (npx,npy) pabb reg
    {-
    -- debugging code
    liftIO $ do
      putStrLn "box: "
      putStrLn $ show box
      putStrLn "pabb: "
      putStrLn $ show pabb
      putStrLn "(spx,spy): "
      putStrLn $ show (spx,spy)
    -}
    return 
      $ R.traverse
          bg
          id
          (\lkup ix@(Z:.i:.j) ->
              if spx <= i && spy <= j && i < lpx && j < lpy
              then
                comp ((rast!(Z:.(i-spx):.(j-spy))) *. (fill . centIxLoc $ ix)) (lkup ix)
              else
                lkup ix
          )
    -}
    --{-
    -- I think this might even be slightly faster still! :)
    rast <- rasterizer (npx,npy) pabb reg
    {-
    -- debugging code
    liftIO $ do
      putStrLn "box: "
      putStrLn $ show box
      putStrLn "pabb: "
      putStrLn $ show pabb
      putStrLn "(spx,spy): "
      putStrLn $ show (spx,spy)
    -}
    return 
      $ R.traverse
          bg
          id
          (\lkup ix@(Z:.i:.j) ->
              let
                base = lkup ix
              in
                if spx <= i && spy <= j && i < lpx && j < lpy
                then
                  let
                    r = rast!(Z:.(i-spx):.(j-spy))
                  in
                    if r < 1e-6 -- computing compositing and fill for everything is more expensive than
                      -- doing this comparison every time
                      -- probably (xD) benchmarking is hard
                    then
                      base
                    else
                      comp (r *. (fill . centIxLoc $ ix)) base
                else
                  base
          )
    --}

data Rasterizable t where
  Rasterizable :: a -> Fill -> Rasterizer a t -> Compositor -> Rasterizable t

data MRasterizable t m where
  MRasterizable :: (GBounded a) => a -> Fill -> MRasterizer a t m -> Compositor -> MRasterizable t m

delayRasterizer :: (Source t Double) => Rasterizer a t -> Rasterizer a R.D
delayRasterizer rsteriz sz bx r = R.delay $ rsteriz sz bx r

delayRaster :: Source t Double => Rasterizable t -> Rasterizable R.D
delayRaster (Rasterizable x f r c) = Rasterizable x f (delayRasterizer r) c

renderLayered :: (Int,Int) -> Box -> [Rasterizable R.D] -> Raster R.D LRGBA
renderLayered sz _ [] = background sz
renderLayered sz box ((Rasterizable reg fill rast comp):lls) = 
  let 
    lflat = renderLayered sz box lls 
  in
    render lflat comp sz box reg fill rast 

mRenderLayered :: (Monad m, MonadIO m) => (Int,Int) -> Box -> [MRasterizable R.D m] -> m (Raster R.D LRGBA)
mRenderLayered sz _ [] = return $ background sz
mRenderLayered sz box ((MRasterizable reg fill rast comp):lls) = do
  lflat <- mRenderLayered sz box lls 
  mrender lflat comp sz box reg fill rast 

renderPicture :: (Int,Int) -> Box -> Picture -> IO (Raster R.D LRGBA)
renderPicture sz bx spics = 
  mRenderLayered
    sz
    bx 
    $ map 
      (\(SimplePicture fl cp) ->
          MRasterizable cp fl scanRasterizer mappend
      )
      spics

renderPictureNoAA :: (Int,Int) -> Box -> Picture -> IO (Raster R.D LRGBA)
renderPictureNoAA sz bx spics = 
  mRenderLayered
    sz
    bx 
    $ map 
      (\(SimplePicture fl cp) ->
          MRasterizable cp fl scanRasterizerNoAA mappend
      )
      spics

toPixel :: LRGBA -> I.Pixel RGB Double
toPixel (LRGBA r g b _) = I.PixelRGB r g b

gammaCorrect :: I.Pixel RGB Double -> I.Pixel RGB Double
gammaCorrect = fmap (** (1/2.2))

convertToImage :: Raster R.D LRGBA -> Image RPU RGB Double
convertToImage raster =
  fromRepaArrayP $ R.map (gammaCorrect . toPixel) $ R.transpose raster 

defaultBox = makeBoxSides (-1) 1 (-1) 1
red = solidFill $ LRGBA 0.5 0 0 0.5
green = solidFill $ LRGBA 0 0.5 0 0.5
blue = solidFill $ LRGBA 0 0 0.5 0.5
orange = solidFill $ LRGBA 0.5 0.3 0 0.5
purple = solidFill $ LRGBA 0.5 0 0.5 0.5
mpurple = solidFill $ LRGBA 0.75 0 0.75 0.75
grey = solidFill $ LRGBA 0.3 0.3 0.3 0.5
circ1 = ((-0.3,0.3)^.from ptAsPair,0.7)^.from circAsPair
circ2 = ((-0.3,-0.3)^.from ptAsPair,0.7)^.from circAsPair
circ3 = ((0.26,0)^.from ptAsPair,0.7)^.from circAsPair

circs = [circ1, circ2, circ3]

purpleQ :: [MRasterizable R.D IO]
purpleQ = [MRasterizable glyphQ mpurple scanRasterizer mappend]


{-
testLayers :: [Rasterizable R.D]
testLayers = zipWith4 (Rasterizable) 
  (circs ++ circs) 
  [orange,purple,grey,red,green,blue] 
  (map delayRasterizer ((replicate 3 (rasterizeBoundary rasterizeCircle)) ++ (replicate 3 rasterizeCircle)))
  (replicate 6 (mappend))
-}
