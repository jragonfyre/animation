--
-- Utils.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Utils
  ( module Utils
  ) where

import Control.Lens
import Control.Lens.TH (defaultFieldRules, generateUpdateableOptics)

onlyGetters = defaultFieldRules & generateUpdateableOptics .~ False

