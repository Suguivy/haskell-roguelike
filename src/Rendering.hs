module Rendering where

import Lens.Micro
import Graphics.Vty
import Linear.V2

import Dungeon
import Game
import Player

renderGame :: Game -> Picture
renderGame g = picForLayers
  [ playerToImg $ g ^. player
  , dungeonToImg $ g ^. dungeon
  ]

dungeonToImg :: Dungeon -> Image
dungeonToImg = vertCat . map (string defAttr . concatMap show) . dungeonToLists

playerToImg :: Player -> Image
playerToImg p = translateX px . translateY py $ char defAttr '@'
  where px = p ^. pos . _x
        py = p ^. pos . _y
