{-# LANGUAGE NegativeLiterals #-}

module Action where

import Graphics.Vty.Input
import Data.Maybe
import Linear.V2

data Action = Move (V2 Int)
            | ExitGame
            | None

bindings :: [(Event, Action)]
bindings =
  [ (EvKey (KChar 'k')  [], Move $ V2 0 -1)
  , (EvKey (KChar 'j')  [], Move $ V2 0 1)
  , (EvKey (KChar 'h')  [], Move $ V2 -1 0)
  , (EvKey (KChar 'l')  [], Move $ V2 1 0)
  , (EvKey (KChar 'y')  [], Move $ V2 -1 -1)
  , (EvKey (KChar 'u')  [], Move $ V2 1 -1)
  , (EvKey (KChar 'b')  [], Move $ V2 -1 1)
  , (EvKey (KChar 'n')  [], Move $ V2 1 1)
  , (EvKey (KChar 'q')  [], ExitGame)
  ]

eventToAction :: Event -> Action
eventToAction e = fromMaybe None (lookup e bindings)
