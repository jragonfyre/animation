--
-- ArclengthApproximator.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.ArcLengthApproximator
  ( module Geometry.ArcLengthApproximator
  ) where

-- needed to approximate
import Geometry.Parametrized 
import Spline
import MathClasses
import Geometry.Affine

import Control.Lens ((^.))

import Data.Vector

--data ArcLengthApproximator = ALApprox
--  { approximated Vector Double
--  }

buildALApproxSpline :: ParamPath1 -> Double -> HSpline
buildALApproxSpline pp1 appstep =
  let
    pp0 = pp1^.param
    (s,e) = pp0^.range
    f = pp0^.func
    fp = pp1^.deriv
    nSamples = ceiling ((e-s)/appstep)
  in
    buildHSpline $
      unfoldrN (nSamples+1)
        (\(curAppAL,ppt,t) ->
            let
              ntbeta = t+appstep
              nt =
                if ntbeta > e 
                then
                  e
                else
                  ntbeta
              npt = f nt
              dL = vectorNorm $ npt-.ppt
              cderiv = vectorNorm $ fp t
              newAppAL = curAppAL + dL
            in
              Just ((t,C1 curAppAL cderiv),(newAppAL,npt,nt))
        )
        (0,f 0,0)
      
    
    



