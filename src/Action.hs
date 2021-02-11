module Action where

import Graphics.Vty.Input
import Control.Applicative
import Data.Maybe

import Direction

data Action = Walk Direction
            | ExitGame
            | None

bindings :: [(Event, Action)]
bindings =
  [ (EvKey KUp         [], Walk N)
  , (EvKey KDown       [], Walk S)
  , (EvKey KLeft       [], Walk W)
  , (EvKey KRight      [], Walk E)
  , (EvKey (KChar 'q') [], ExitGame)
  ]

eventToAction :: Event -> Action
eventToAction e = fromJust $ lookup e bindings <|> Just None
