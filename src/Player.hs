{-# LANGUAGE TemplateHaskell #-}

module Player where

import Lens.Micro.TH (makeLenses)

data Player = Player
  { _pos :: (Int, Int)
  }

makeLenses ''Player
