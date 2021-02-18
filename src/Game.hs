{-# LANGUAGE TemplateHaskell, NegativeLiterals #-}

module Game where

import Lens.Micro.TH
import Lens.Micro
import Linear.V2

import Dungeon
import Player
import Direction
import Action

data Game = Game
  { _dungeon :: Dungeon
  , _player :: Player
  }

makeLenses ''Game

newGame :: IO Game
newGame = do
  dun <- makeDungeonFromFile "maps/test.map"
  return $ Game dun (Player $ V2 0 0)

runAction :: Action -> Game -> Maybe Game
runAction (Walk N) game = Just $ game & player . pos +~ V2 0 -1
runAction (Walk S) game = Just $ game & player . pos +~ V2 0 1
runAction (Walk W) game = Just $ game & player . pos +~ V2 -1 0
runAction (Walk E) game = Just $ game & player . pos +~ V2 1 0
runAction (Walk NW) game = Just $ game & player . pos +~ V2 -1 -1
runAction (Walk NE) game = Just $ game & player . pos +~ V2 1 -1
runAction (Walk SW) game = Just $ game & player . pos +~ V2 -1 1
runAction (Walk SE) game = Just $ game & player . pos +~ V2 1 1
runAction None g = Just g
runAction ExitGame _ = Nothing
