module Rendering where

import Lens.Micro
import Graphics.Vty

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
playerToImg p = (translateX px . translateY py $ char defAttr '@') <|> string defAttr ("                      (" ++ show px ++ "," ++ show py ++ ")")
  where px = p ^. pos . _1
        py = p ^. pos . _2
