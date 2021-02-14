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
  [ (EvKey (KChar 'k')  [], Walk N)
  , (EvKey (KChar 'j')  [], Walk S)
  , (EvKey (KChar 'h')  [], Walk W)
  , (EvKey (KChar 'l')  [], Walk E)
  , (EvKey (KChar 'y')  [], Walk NW)
  , (EvKey (KChar 'u')  [], Walk NE)
  , (EvKey (KChar 'b')  [], Walk SW)
  , (EvKey (KChar 'n')  [], Walk SE)
  , (EvKey (KChar 'q')  [], ExitGame)
  ]

eventToAction :: Event -> Action
eventToAction e = fromJust $ lookup e bindings <|> Just None
