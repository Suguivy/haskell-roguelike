{-# LANGUAGE TemplateHaskell #-}

module Player where

import Lens.Micro.TH (makeLenses)

data Player = Player
  { _x :: Int
  , _y :: Int
  }

makeLenses ''Player
