module Geometry.Ellipse
  ( module Geometry.Ellipse
  ) where

import Geometry.Types
import Geometry.Affine

import Data.Maybe (fromJust)

import Control.Lens.TH
import Control.Lens ((&),(^.),from,_1)

import Utils

data Ellipse = Ellipse
  { _ellipseCenter :: Point
  , _ellipseMatrix :: (Matrix,Double)
  , _ellipseRadX :: Double
  , _ellipseRadY :: Double
  , _ellipsePhi :: Double
  }
  deriving (Show,Eq,Ord,Read)

makeFields ''Ellipse

data EllipticalArc = EllipticalArc
  { _ellipticalArcEllipse :: Ellipse
  , _ellipticalArcStart :: Double -- theta start
  , _ellipticalArcDelta :: Double -- the difference thetaEnd-thetaStart
  }
  deriving (Show,Eq,Ord,Read)

makeFields ''EllipticalArc

instance Geometric Ellipse where
  transform aff Ellipse{_ellipseCenter=c,_ellipseMatrix=(mat,tol)}=
    let
      (m,t) = aff^.affAsPair
      mi = invertMatrix m
    in
      makeEllipseFromMatrix 
        (transform aff c)
        (((transpose mi)*.mat*.mi),tol)

instance GBounded Ellipse where
  bounds ell = 
    let
      cent = ell^.center
      ((a,c),(b,d)) = ell^.matrix._1.matAsComponents
      -- a /= 0, d /= 0
      -- a x^2 + (b+c) xy + dy^2 = 1
      -- 2axx' + (b+c) x'y + (b+c) xy' +2dyy' = 0
      -- x'=0,y'/=0 => (b+c)x + 2dy = 0
      -- y = -(b+c)x/2d 
      -- so we have ax^2 - (b+c)^2/2d x^2 + (b+c)^2/4d x^2 = 1
      bc = (b+c)^2/4
      -- let alpha = (b+c)^2/4d
      alpha = bc/d
      -- x = pm sqrt (1/(a-alpha))
      xdelt = sqrt (1/(a-alpha))
      -- x'/=0,y'=0 => 2ax + (b+c)y = 0
      -- x = -(b+c)y/2a
      -- y^2((b+c)^2/4a - (b+c)^2/2a + d) = 1
      -- beta = (b+c)^2/4a
      beta = bc/a
      -- y = pm sqrt 
      ydelt = sqrt (1/(d-beta))
    in
      makeBoxCenter cent xdelt ydelt

pointAngle :: Ellipse -> Point -> Double
pointAngle ell pt = 
  let
    c = ell^.center
    ph = ell^.phi
    rx = ell^.radX
    ry = ell^.radY
    vec = pt -. c
    v = (rotate (-ph)) *. vec
    (vx,vy)=v^.vecAsPair
    sVec=(vx/rx,vy/ry)^.from vecAsPair
  in
    angle xVec sVec

ensureAngleSignSameAs :: Double -> Double -> Double
ensureAngleSignSameAs ang sgn = 
  if sgn < 0
  then
    if ang <= 0
    then
      ang
    else
      ang + 2*pi
  else
    if ang >= 0
    then
      ang
    else
      ang - 2*pi


instance Geometric EllipticalArc where
  transform aff earc =
    let
      ell = earc^.ellipse
      (m,_) = aff^.affAsPair
      st = parametrizeEArc earc 0
      ed = parametrizeEArc earc 1
      nst = aff*.st
      ned = aff*.ed
      nell = transform aff ell
      th1 = pointAngle nell nst
      th2 = pointAngle nell ned
      thDelta = th2-th1
      d = det m
      delt = ensureAngleSignSameAs thDelta (d*(earc^.delta))
    in
      EllipticalArc nell th1 delt

instance GBounded EllipticalArc where
  bounds = bounds . (^.ellipse)

makeEllipseFromRadii :: Double -> Point -> (Double, Double, Double) -> Ellipse
makeEllipseFromRadii tol pt rs@(rx,ry,rphi) = 
  let
    mat = ellipseRadiiAndRotationToMatrix rs
  in
    Ellipse pt (mat,tol) rx ry rphi

makeEllipseFromMatrix :: Point -> (Matrix,Double) -> Ellipse
makeEllipseFromMatrix pt (mat,tol) = 
  let
    (rx,ry,rphi) = ellipseMatrixToRadiiToleranceUnsafe tol mat
  in
    Ellipse pt (mat,tol) rx ry rphi

parametrizeEArc :: EllipticalArc -> Double -> Point
parametrizeEArc earc t = 
  let
    ell = earc^.ellipse
    c = ell^.center
    rx = ell^.radX
    ry = ell^.radY
    ph = ell^.phi
    st = earc^.start
    delt = earc^.delta
    rot = rotate ph
    theta = t*delt + st
  in
    c +. rot *. (makeVector (rx * cos theta) (ry * sin theta))

derivativeEArc :: EllipticalArc -> Double -> Vector
derivativeEArc earc t =
  let
    ell = earc^.ellipse
    rx = ell^.radX
    ry = ell^.radY
    ph = ell^.phi
    st = earc^.start
    delt = earc^.delta
    rot = rotate ph
    theta = t*delt + st
  in
    rot *. (makeVector (-rx * delt * sin theta) (ry * delt * cos theta))

{-
indicate :: Bool -> Double
indicate True = 1
indicate False = 0

sign :: Double -> Double
sign x | x >= 0 = 1.0
       | otherwise = -1.0
-}

angle :: Vector -> Vector -> Double
angle  = angleFrom

xVec :: Vector
xVec = makeVector 1 0

makeEArcMatrixEndpoints :: Point -> (Matrix,Double) -> Bool -> Bool -> Point -> EllipticalArc
makeEArcMatrixEndpoints s (mat,tol) fA fS e =
  makeEArcRadiiEndpoints tol s (ellipseMatrixToRadiiToleranceUnsafe tol mat) fA fS e

makeEArcRadiiEndpoints :: Double -> Point -> (Double,Double,Double) -> Bool -> Bool -> Point -> EllipticalArc
makeEArcRadiiEndpoints tol s (rx,ry,rphi) fA fS e = endpointParamToCenterParam tol s e rx ry rphi fA fS

endpointParamToCenterParam :: Double -> Point -> Point -> Double -> Double -> Double -> Bool -> Bool -> EllipticalArc
endpointParamToCenterParam tol st ed rxi ryi rphi fA fS =
  let
    mp = line st ed 0.5
    aff1 = translateToOrigin mp
    r = rotate (negate rphi)
    aff = r *. aff1
    affi = invertAffine aff
    (x1p,y1p) = (aff *. st)^.ptAsPair
    x1p2=x1p^2
    y1p2=y1p^2
    -- ensure radii are positive. Can't ensure that the radii are nonzero in this particular function
    rxint = abs rxi 
    ryint = abs ryi
    rxin2 = rxint^2
    ryin2 = ryint^2
    lambda = x1p2/rxin2 + y1p2/ryin2 
    sqlam = sqrt lambda
    (rx,ry) =
      if lambda <= 1 -- the svg radius out of bounds adjustment.
      then
        (rxint,ryint)
      else
        (sqlam * rxint, sqlam * ryint)
    rx2=rx^2
    ry2=ry^2
    signScal = 2*(indicate (fA /= fS)) - 1
    scalar = signScal * (sqrt $ (abs (rx2*ry2-rx2*y1p2-ry2*x1p2))/(rx2*y1p2+ry2*x1p2))
    vec = makeVector (rx*y1p/ry) (-ry*x1p/rx)
    v2 = scalar*.vec
    centP = v2^.vecAsPair.from ptAsPair
    (cxp,cyp) = centP^.ptAsPair
    cent = affi *. centP
    vst = makeVector ((x1p-cxp)/rx) ((y1p-cyp)/ry)
    ved = makeVector ((-x1p-cxp)/rx) ((-y1p-cyp)/ry)
    thS = angle xVec vst
    dthetainit = angle vst ved
    dtheta = 
      if fS
      then
        if dthetainit <= 0
        then
          dthetainit + 2*pi
        else
          dthetainit
      else
        if dthetainit >= 0
        then
          dthetainit - 2*pi
        else
          dthetainit
  in
    EllipticalArc
      ( makeEllipseFromRadii
          tol
          cent
          ( rx
          , ry
          , rphi
          )
      )
      thS
      dtheta

-- the eqn of the ellipse given the mat is (x,y) mat (x,y)^t == 1
-- assumes matrix is symmetric! Actually, jk we can symmetrize it? Yep, nvm.
-- suffers hugely from rounding error
ellipseMatrixToRadiiAndRotation :: Matrix -> Maybe (Double,Double,Double)
ellipseMatrixToRadiiAndRotation mat = 
  let
    symMat = symmetrizeMatrix mat
    matEvecs = matrixEigenvectors symMat
  in
    case matEvecs of 
      [(ev1,ax1),(ev2,ax2)] ->
        let 
          r1 = sqrt (1/ev1)
          r2 = sqrt (1/ev2)
        in
          Just $ 
            if r1 > r2
            then
              (r1,r2,angle xVec ax1)
            else
              (r2,r1,angle xVec ax2)
      _ ->
        Nothing

ellipseMatrixToRadiiTolerance :: Double -> Matrix -> Maybe (Double,Double,Double)
ellipseMatrixToRadiiTolerance tol mat = 
  let
    symMat = symmetrizeMatrix mat
    matEvecs = matrixEigenvectorsTolerance tol symMat
  in
    case matEvecs of 
      [(ev1,ax1),(ev2,ax2)] ->
        let 
          r1 = sqrt (1/ev1)
          r2 = sqrt (1/ev2)
          a11 = angle xVec ax1
          a12 = angle (negify xVec) ax1
          a21 = angle xVec ax2
          a22 = angle (negify xVec) ax2
        in
          Just $ 
            if r1 > r2
            then
              (r1,r2,if (abs a11) < (abs a12) then a11 else a12)
            else
              (r2,r1,if (abs a21) < (abs a22) then a21 else a22)
      _ ->
        Nothing

ellipseMatrixToRadiiToleranceUnsafe :: Double -> Matrix -> (Double,Double,Double)
ellipseMatrixToRadiiToleranceUnsafe tol mat = case ellipseMatrixToRadiiTolerance tol mat of
  Just rs -> 
    rs

ellipseMatrixToRadiiAndRotationUnsafe :: Matrix -> (Double,Double,Double)
ellipseMatrixToRadiiAndRotationUnsafe mat = case ellipseMatrixToRadiiAndRotation mat of
  Just rs ->
    rs

ellipseRadiiAndRotationToMatrix :: (Double,Double,Double) -> Matrix
ellipseRadiiAndRotationToMatrix (rx,ry,rphi) =
  let
    rot1 = rotate rphi
    rot2 = rotate (-rphi)
    m = diagonal (1/rx^2) (1/ry^2)
  in
    rot1 *. m *. rot2

ellipseRadiiToMatrix = ellipseRadiiAndRotationToMatrix
