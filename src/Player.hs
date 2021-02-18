{-# LANGUAGE TemplateHaskell #-}

module Player where

import Lens.Micro.TH (makeLenses)
import Linear.V2

data Player = Player
  { _pos :: V2 Int
  }

makeLenses ''Player
