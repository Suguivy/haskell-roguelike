module Dungeon where

import Data.Matrix
import System.Random

data Cell = Solid | Empty

instance Show Cell where
  show Solid = "#"
  show Empty = "."

newtype Dungeon = Dungeon (Matrix Cell)

instance Show Dungeon where
  show (Dungeon m) = unlines . map (concatMap show) $ toLists m

makeDungeon :: (RandomGen r) => r -> Int -> Int -> Dungeon
makeDungeon gen w h = Dungeon $ Data.Matrix.fromList h w (randomCells gen)
  where randomCells g = let (c,nGen) = randomR (0 :: Int,1 :: Int) g
                        in case c of 0 -> Solid : randomCells nGen
                                     1 -> Empty : randomCells nGen


dungeonToLists :: Dungeon -> [[Cell]]
dungeonToLists (Dungeon m) = toLists m
