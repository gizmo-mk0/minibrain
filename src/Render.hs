module Render where

import qualified SDL
import qualified SDL.Primitive as SDLP

import Types

class Renderable a where
    render :: a -> SdlData -> IO ()

instance Renderable Perceptron where
    render :: Perceptron -> SdlData -> IO ()
    render perceptron sdlData = undefined
        where
        renderPin :: (Pin a) => a -> IO ()
        renderPin = undefined

instance Renderable Connector where
    render :: Connector -> SdlData -> IO ()
    render connector sdlData = undefined