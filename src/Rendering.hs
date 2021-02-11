module Rendering where

import Graphics.Vty
import Dungeon
import Game
import Player

renderGame :: Game -> Picture
renderGame g = picForLayers
  [ playerToImg (getPlayer g)
  , dungeonToImg (getDungeon g)
  ]

dungeonToImg :: Dungeon -> Image
dungeonToImg = vertCat . map (string defAttr . concatMap show) . dungeonToLists

playerToImg :: Player -> Image
playerToImg (Player (x,y)) = translateX x . translateY y $ char defAttr '@'
