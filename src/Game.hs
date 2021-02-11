module Game where

import Dungeon
import Player
import Direction
import Action

data Game = Game
  { getDungeon :: Dungeon
  , getPlayer :: Player
  }

newGame :: Game
newGame = Game (makeDungeon 30 10) (Player (1,1))

runAction :: Action -> Game -> Maybe Game
runAction (Walk N) (Game d (Player (x,y))) = Just $ Game d (Player (x,y-1))
runAction (Walk S) (Game d (Player (x,y))) = Just $ Game d (Player (x,y+1))
runAction (Walk W) (Game d (Player (x,y))) = Just $ Game d (Player (x-1,y))
runAction (Walk E) (Game d (Player (x,y))) = Just $ Game d (Player (x+1,y))
runAction None g = Just g
runAction ExitGame _ = Nothing
