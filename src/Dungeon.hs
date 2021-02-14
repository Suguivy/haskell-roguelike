module Dungeon where

import Data.Matrix

data Cell = Solid | Empty

instance Show Cell where
  show Solid = "#"
  show Empty = "."

newtype Dungeon = Dungeon (Matrix Cell)

instance Show Dungeon where
  show (Dungeon m) = unlines . map (concatMap show) $ toLists m

makeDungeon :: Int -> Int -> Dungeon
makeDungeon w h = Dungeon $ matrix h w $ const Empty

dungeonToLists :: Dungeon -> [[Cell]]
dungeonToLists (Dungeon m) = toLists m
