module Dungeon.Loader where

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)

loadDungeon :: String -> IO (Either String Object)
loadDungeon = eitherDecodeFileStrict
