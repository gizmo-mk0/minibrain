{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Primitive as SDLP

import Control.Monad (unless)

import Types
import Globals

main :: IO ()
main = do
    -- Init SDL
    SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
    window   <- SDL.createWindow "MiniBrain" SDL.defaultWindow
    surface  <- SDL.getWindowSurface window
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    -- Load resources
    let cfg = Config
    -- Run main loop
    runMinibrain cfg mainLoop
    -- Free resources
    -- Uninitialize SDL
    SDL.destroyWindow window
    SDL.quit

runMinibrain _ = undefined

-- :: MonadReader Config m, MonadState Vars m, Audio m, AudioSfx m, Logger m,
-- Clock m, CameraControl m, Renderer m, HasInput m, Title m, Play m, Pause m,
-- GameOver m
mainLoop :: Minibrain ()
mainLoop = do
    -- Collect input
    -- Scene logic
    -- Render
    -- Decide next scene
    let quit = False
    unless quit mainLoop

-- initializeEnvironment :: IO SdlData
-- initializeEnvironment = do
--     SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
--     window   <- SDL.createWindow "MiniBrain" SDL.defaultWindow
--     surface  <- SDL.getWindowSurface window
--     renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
--     return (SdlData (SdlGraphicsData window surface renderer))

-- initializeGame :: GameData
-- initializeGame = GameData (Editor (EditorData []))

-- runGameLoop :: SdlData -> GameData -> IO ()
-- runGameLoop sdlData gameData = do
--     events <- SDL.pollEvents
--     let newGameData@(GameData newGameState) =
--             runLogic $ foldl (flip handleEvent) gameData events
--     render gameData sdlData
--     if newGameState == Quit
--         then return ()
--         else runGameLoop sdlData newGameData

-- handleEvent :: SDL.Event -> GameData -> GameData
-- handleEvent e gd =
--     case SDL.eventPayload e of
--         SDL.KeyboardEvent kbe ->
--             case SDL.keyboardEventKeysym kbe of
--                 SDL.Keysym _ SDL.KeycodeEscape _ -> GameData Quit
--                 _ -> gd
--         _ -> gd

-- runLogic :: GameData -> GameData
-- runLogic gd = gd