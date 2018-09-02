--
-- Spline.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Spline
  ( module Spline
  ) where

-- I need a generalized cubic spline, and ways to produce and evaluate them.
-- Shoud handle 1D case (producing C1 splines, tangents (er derivatives) are equal across knots) as well as 
-- 2D case (producing G1 splines, tangents are parallel across knots) also C1 splines are sometimes necessary
-- for 2D case as well, but much less often.

-- desired supported interpolation methods:
-- Catmull/Rom (Uniform,Centripetal,Chordal) see wiki (2D/C1)
-- Monotone cubic interpolation (1D)
-- Finite Difference (2D/C1) (Maybe do weighted finite difference to produce G1 curve?)
-- Cardinal spline 
-- kochanek-Bartels spline (2D, continuity varies)

-- would be good to have efficient 4D matrices/vectors for this.
-- Look into SIMD
-- also uhhhh maybe we should be using Float instead of Double?
-- Anyway, that's super low priority. First a good interface is necessary.
-- Less low priority is of course the actual 4d matrices/vectors. Those are crucial to improve.
-- Hm. Maybe switch to the linear library by edward kmett?

