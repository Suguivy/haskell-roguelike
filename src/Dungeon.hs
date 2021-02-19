{-# LANGUAGE OverloadedStrings #-}
module Dungeon where

import Data.Aeson
import Data.Aeson.Types
import Data.Matrix
import Linear.V2
import Data.Tuple
import Data.Maybe

data Cell = Solid
          | Empty
          deriving (Eq)

instance Show Cell where
  show cell = [fromMaybe '?' (lookup cell cellMapping)]

cellMapping :: [(Cell, Char)]
cellMapping =
  [ (Empty, '.')
  , (Solid, '#')
  ]

newtype Dungeon = Dungeon (Matrix Cell)

instance Show Dungeon where
  show (Dungeon m) = unlines . map (concatMap show) $ toLists m

makeDungeonFromFile :: String -> IO Dungeon
makeDungeonFromFile f = do
  --contents <- readFile f
  ef <- eitherDecodeFileStrict f :: IO (Either String Object)
  let obj = case ef of (Right o) -> o
                       (Left l) -> error l
  let stringMap = case parse (.: "map") obj :: Result [String] of
        (Success m) -> m
        (Error s) -> error s
  let cellMappingR = map swap cellMapping
      charToCell c = fromMaybe (error "Invalid cell in the .map file") (c `lookup` cellMappingR)
      cellLists = map charToCell <$> stringMap
  return . Dungeon . fromLists $ cellLists

dungeonToLists :: Dungeon -> [[Cell]]
dungeonToLists (Dungeon m) = toLists m

getCell :: V2 Int -> Dungeon -> Cell
getCell (V2 x y) (Dungeon m) = fromMaybe Solid (safeGet (y+1) (x+1) m)
