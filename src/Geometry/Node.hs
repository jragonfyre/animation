--
-- Node.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Node
  ( module Geometry.Node
  ) where

import Geometry.Types
import Geometry.Affine


-- nodes are coordinate system trees with objects at each node
data Node a = Node 
  { toParent :: Affine
  , subNodes :: [Node a]
  , children :: [a]
  }

flatten :: (Geometric a) => Node a -> [a]
flatten = flattenWith (makeAffine unit zero)

flattenWith :: (Geometric a) => Affine -> Node a -> [a]
flattenWith toWorldFromParent Node{..} =
  let
    toWorld = toWorldFromParent *. toParent
  in
    concat $ (map (transform toWorld) children):(map (flattenWith toWorld) subNodes)
  

