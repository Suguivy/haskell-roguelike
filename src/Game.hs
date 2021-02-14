{-# LANGUAGE TemplateHaskell #-}

module Game where

import System.Random
import Lens.Micro.TH
import Lens.Micro

import Dungeon
import Player
import Direction
import Action

data Game = Game
  { _dungeon :: Dungeon
  , _player :: Player
  }

makeLenses ''Game

newGame :: (RandomGen r) => r -> Game
newGame gen = Game (makeDungeon gen 30 10) (Player (0,0))

runAction :: Action -> Game -> Maybe Game
runAction (Walk N) game = Just $ game & player . pos . _2 -~ 1
runAction (Walk S) game = Just $ game & player . pos . _2 +~ 1
runAction (Walk W) game = Just $ game & player . pos . _1 -~ 1
runAction (Walk E) game = Just $ game & player . pos . _1 +~ 1
runAction (Walk NW) game = Just $ game & player . pos . both -~ 1
runAction (Walk NE) game = Just $ game & player . pos %~ (\(x,y) -> (x+1, y-1))
runAction (Walk SW) game = Just $ game & player . pos %~ (\(x,y) -> (x-1, y+1))
runAction (Walk SE) game = Just $ game & player . pos . both +~ 1
runAction None g = Just g
runAction ExitGame _ = Nothing
