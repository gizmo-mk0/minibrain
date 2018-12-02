{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Primitive as SDLP

import Types
import Globals

main :: IO ()
main = do
    sdlData   <- initializeEnvironment
    runGameLoop sdlData initializeGame

initializeEnvironment :: IO SdlData
initializeEnvironment = do
    SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
    window   <- SDL.createWindow "MiniBrain" SDL.defaultWindow
    surface  <- SDL.getWindowSurface window
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    return (SdlData (SdlGraphicsData window surface renderer))

initializeGame :: GameData
initializeGame = GameData Menu

runGameLoop :: SdlData -> GameData -> IO ()
runGameLoop sdlData gameData = do
    events <- SDL.pollEvents
    let newGameData@(GameData newGameState) = runLogic $ foldl (flip handleEvent) gameData events
    render sdlData gameData
    if newGameState == Quit
        then return ()
        else runGameLoop sdlData newGameData

handleEvent :: SDL.Event -> GameData -> GameData
handleEvent e gd =
    case SDL.eventPayload e of
        SDL.KeyboardEvent kbe ->
            case SDL.keyboardEventKeysym kbe of
                SDL.Keysym _ SDL.KeycodeEscape _ -> GameData Quit
                _ -> gd
        _ -> gd

runLogic :: GameData -> GameData
runLogic gd = gd

render :: SdlData -> GameData -> IO ()
render sdlData gameData = do
    let (SdlData (SdlGraphicsData _ _ r)) = sdlData
    SDL.rendererDrawColor r SDL.$= editorBackgroundColor
    SDL.clear r
    SDLP.thickLine r (SDL.V2 10 15) (SDL.V2 10 120) 3 (SDL.V4 255 255 255 255)
    SDL.present r