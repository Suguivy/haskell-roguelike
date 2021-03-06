module Main where

import Graphics.Vty

import Game
import Rendering
import Action

main :: IO ()
main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    game <- newGame
    loop vty game
    shutdown vty
    where loop vty game = do
            update vty $ renderGame game
            e <- nextEvent vty
            let action = eventToAction e
                nGame = runAction action game
            case nGame of
              Nothing -> return ()
              Just ng -> loop vty ng
