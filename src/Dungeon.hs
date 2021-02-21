{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Dungeon where

import Data.Aeson
import Data.Matrix
import Linear.V2
import Data.Tuple
import Data.Maybe
import Lens.Micro.TH
import Lens.Micro

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

data Dungeon = Dungeon
  { _name   :: String
  , _layout :: Matrix Cell
  }

makeLenses ''Dungeon

instance Show Dungeon where
  show dun = unlines . map (concatMap show) $ toLists $ dun ^. layout

instance FromJSON Dungeon where
  parseJSON = withObject "Dungeon" $ \v -> do
    mapName <- v .: "name"
    stringMap <- v .: "layout"
    let cellMappingR = map swap cellMapping
        charToCell c = fromMaybe (error "Invalid cell in the .map file") (c `lookup` cellMappingR)
        cellLists = map charToCell <$> stringMap
    return $ Dungeon mapName (fromLists cellLists)


makeDungeonFromFile :: FilePath -> IO Dungeon
makeDungeonFromFile f = do
  eithDun <- eitherDecodeFileStrict f
  return $ case eithDun of
    Left err -> error err
    Right dun -> dun

dungeonToLists :: Dungeon -> [[Cell]]
dungeonToLists dun = toLists $ dun ^. layout

getCell :: V2 Int -> Dungeon -> Cell
getCell (V2 x y) dun = fromMaybe Solid (safeGet (y+1) (x+1) $ dun ^. layout)
