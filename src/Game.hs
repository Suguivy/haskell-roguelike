{-# LANGUAGE TemplateHaskell #-}

module Game where

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

newGame :: Game
newGame = Game (makeDungeon 30 10) (Player 0 0)

runAction :: Action -> Game -> Maybe Game
runAction (Walk N) game = Just $ game & player . y %~ (-)1
runAction (Walk S) game = Just $ game & player . y %~ (+)1
runAction (Walk W) game = Just $ game & player . x %~ (-)1
runAction (Walk E) game = Just $ game & player . x %~ (+)1
runAction None g = Just g
runAction ExitGame _ = Nothing
