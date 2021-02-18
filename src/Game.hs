{-# LANGUAGE TemplateHaskell, NegativeLiterals #-}

module Game where

import Lens.Micro.TH
import Lens.Micro
import Linear.V2

import Dungeon
import Player
import Action

data Game = Game
  { _dungeon :: Dungeon
  , _player :: Player
  }

makeLenses ''Game

newGame :: IO Game
newGame = do
  dun <- makeDungeonFromFile "maps/test.map"
  return $ Game dun (Player $ V2 1 1)

runAction :: Action -> Game -> Maybe Game
runAction (Move vec) game =
  if ableToMove then Just $ game & player . pos .~ newPos
                else Just game
  where ableToMove = getCell newPos (game ^. dungeon) == Empty
        newPos = (game ^. player . pos) + vec
runAction None g = Just g
runAction ExitGame _ = Nothing
