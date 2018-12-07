{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import qualified SDL.Primitive as SDLP

import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (evalStateT)

import Types
import Globals

main :: IO ()
main = do
    -- Init SDL
    SDL.initialize [SDL.InitVideo] -- TODO catch SDLException
    window   <- SDL.createWindow "MiniBrain" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    -- Load resources
    let cfg = Config window renderer
        gameData = initData
    -- Run main loop
    runMinibrain cfg gameData mainLoop
    -- Free resources
    -- Uninitialize SDL
    SDL.destroyWindow window
    SDL.quit

initData :: GameData
initData = GameData (SceneData Title TitleData BriefingData EditorData SimulationData) CameraData InputData

runMinibrain :: Config -> GameData -> Minibrain a -> IO a
runMinibrain config gameData (Minibrain m) =
    evalStateT (runReaderT m config) gameData

mainLoop :: Minibrain ()
mainLoop = do
    -- Collect input
    -- Scene logic
    -- Render
    renderScene
    -- Decide next scene
    let quit = False
    unless quit mainLoop

renderScene :: Minibrain()
renderScene = undefined

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