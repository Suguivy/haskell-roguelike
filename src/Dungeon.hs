module Dungeon where

import Data.Matrix hiding ((<|>))
import Linear.V2
import Data.Tuple
import Data.Maybe
import Control.Applicative ((<|>))

data Cell = Solid
          | Empty
          deriving (Eq)

instance Show Cell where
  show cell = [fromJust (lookup cell cellChars <|> Just '?')]

cellChars :: [(Cell, Char)]
cellChars =
  [ (Empty, '.')
  , (Solid, '#')
  ]

newtype Dungeon = Dungeon (Matrix Cell)

instance Show Dungeon where
  show (Dungeon m) = unlines . map (concatMap show) $ toLists m

makeDungeonFromFile :: String -> IO Dungeon
makeDungeonFromFile f = do
  contents <- readFile f
  return $ Dungeon $ fromLists $ map (fromJust . (`lookup` map swap cellChars)) <$> lines contents

dungeonToLists :: Dungeon -> [[Cell]]
dungeonToLists (Dungeon m) = toLists m

getCell :: V2 Int -> Dungeon -> Cell
getCell (V2 x y) (Dungeon m) = fromMaybe Solid (safeGet (y+1) (x+1) m)
