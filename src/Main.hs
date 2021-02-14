module Main where

import Graphics.Vty
import System.Random

import Game
import Rendering
import Action

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    gen <- getStdGen
    loop vty $ newGame gen
    shutdown vty
    where loop vty game = do
            update vty $ renderGame game
            e <- nextEvent vty
            let action = eventToAction e
                nGame = runAction action game
            case nGame of
              Nothing -> return ()
              Just ng -> loop vty ng
