module Render where

import qualified SDL
import qualified SDL.Primitive as SDLP

import Types
import Globals

class Renderable a where
    render :: a -> SdlData -> IO ()

instance Renderable Perceptron where
    -- render :: Perceptron -> SdlData -> IO ()
    render perceptron sdlData = undefined
        where
        renderPin :: (Pin a) => a -> IO ()
        renderPin = undefined

instance Renderable Connector where
    -- render :: Connector -> SdlData -> IO ()
    render connector sdlData = undefined

instance Renderable GameData where
    render (GameData gameScene) sdlData = render gameScene sdlData

instance Renderable GameScene where
    render (Menu       menuData)       sdlData = render menuData       sdlData
    render (Briefing   briefingData)   sdlData = render briefingData   sdlData
    render (Editor     editorData)     sdlData = render editorData     sdlData
    render (Simulation simulationData) sdlData = render simulationData sdlData
    render Quit                        _       = return ()

instance Renderable MenuData where
    render = undefined
instance Renderable BriefingData where
    render = undefined
instance Renderable EditorData where
    render (EditorData editorData) sdlData = do
        let (SdlData (SdlGraphicsData _ _ r)) = sdlData
        SDL.rendererDrawColor r SDL.$= editorBackgroundColor
        SDL.clear r
        SDLP.thickLine r (SDL.V2 10 15) (SDL.V2 10 120) 3 (SDL.V4 255 255 255 255)
        SDL.present r
instance Renderable SimulationData where
    render = undefined