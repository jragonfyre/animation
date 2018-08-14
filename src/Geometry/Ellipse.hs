module Geometry.Ellipse
  ( module Geometry.Ellipse
  ) where

import Geometry.Types
import Geometry.Affine

import Data.Maybe (fromJust)

import Control.Lens.TH
import Control.Lens ((&),(^.),from)

data Ellipse = Ellipse
  { _ellipseCenter :: Point
  , _ellipseMatrix :: Matrix
  , _ellipseRadX :: Double
  , _ellipseRadY :: Double
  , _ellipsePhi :: Double
  }

makeFields ''Ellipse

data EllipticalArc = EllipticalArc
  { _ellipticalArcEllipse :: Ellipse
  , _ellipticalArcStart :: Double -- theta start
  , _ellipticalArcDelta :: Double -- the difference thetaEnd-thetaStart
  }

makeFields ''EllipticalArc

instance Geometric Ellipse where
  transform aff Ellipse{_ellipseCenter=c,_ellipseMatrix=mat}=
    let
      (m,t) = aff^.affAsPair
      mi = invertMatrix m
    in
      makeEllipseFromMatrix 
        (transform aff c)
        ((transpose mi)*.mat*.mi)

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


makeEllipseFromRadii :: Point -> (Double, Double, Double) -> Ellipse
makeEllipseFromRadii pt rs@(rx,ry,rphi) = 
  let
    mat = ellipseRadiiAndRotationToMatrix rs
  in
    Ellipse pt mat rx ry rphi

makeEllipseFromMatrix :: Point -> Matrix -> Ellipse
makeEllipseFromMatrix pt mat = 
  let
    (rx,ry,rphi) = ellipseMatrixToRadiiAndRotationUnsafe mat
  in
    Ellipse pt mat rx ry rphi

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
    theta = t*delt + st - ph
  in
    c+.(makeVector (rx * cos theta) (ry * sin theta))

derivativeEArc :: EllipticalArc -> Double -> Vector
derivativeEArc earc t =
  let
    ell = earc^.ellipse
    rx = ell^.radX
    ry = ell^.radY
    ph = ell^.phi
    st = earc^.start
    delt = earc^.delta
    theta = t*delt + st - ph
  in
    makeVector (-rx * delt * sin theta) (ry * delt * cos theta)

indicate :: Bool -> Double
indicate True = 1
indicate False = 0

sign :: Double -> Double
sign x | x >= 0 = 1.0
       | otherwise = -1.0

angle :: Vector -> Vector -> Double
angle u v = (sign (cross u v)) * (acos ((u`dot`v)/((vectorNorm u) * (vectorNorm v))))

xVec :: Vector
xVec = makeVector 1 0

endPointParamToCenterParam :: Point -> Point -> Double -> Double -> Double -> Bool -> Bool -> EllipticalArc
endPointParamToCenterParam st ed rxi ryi phi fA fS =
  let
    mp = line st ed 0.5
    aff1 = translateToOrigin mp
    r = rotate (negate phi)
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
    scalar = (indicate (fA /= fS))* (sqrt $ (rx2*ry2-rx2*y1p2-ry2*x1p2)/(rx2*y1p2+ry2*x1p2))
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
          dtheta + 2*pi
        else
          dtheta
      else
        if dthetainit >= 0
        then
          dtheta - 2*pi
        else
          dtheta
  in
    EllipticalArc
      ( makeEllipseFromRadii
          cent
          ( rx
          , ry
          , phi
          )
      )
      thS
      dtheta

-- the eqn of the ellipse given the mat is (x,y) mat (x,y)^t == 1
-- assumes matrix is symmetric! Actually, jk we can symmetrize it? Yep, nvm.
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

ellipseMatrixToRadiiAndRotationUnsafe :: Matrix -> (Double,Double,Double)
ellipseMatrixToRadiiAndRotationUnsafe = fromJust . ellipseMatrixToRadiiAndRotation

ellipseRadiiAndRotationToMatrix :: (Double,Double,Double) -> Matrix
ellipseRadiiAndRotationToMatrix (rx,ry,rphi) =
  let
    rot1 = rotate rphi
    rot2 = rotate (-rphi)
    m = diagonal (1/rx^2) (1/ry^2)
  in
    rot1 *. m *. rot2

