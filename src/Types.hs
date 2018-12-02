{-# LANGUAGE ExistentialQuantification #-}

module Types where

import qualified SDL

data Perceptron = forall a . Pin a => Perceptron
                { id       :: Int
                , position :: (SDL.V2 Int)
                , pins     :: [a] }

data InputPin = InputPin Connector Perceptron
data OutputPin = OutputPin Connector Perceptron

data Connector = Connector OutputPin InputPin

class Pin a where
    getConnector :: a -> Connector

instance Pin InputPin where
    getConnector (InputPin c _) = c

instance Pin OutputPin where
    getConnector (OutputPin c _) = c

data GameState       = Menu | Editor | Game | Quit deriving (Eq)
data GameData        = GameData GameState
data SdlData         = SdlData SdlGraphicsData
data SdlGraphicsData = SdlGraphicsData
                     { sdlWindow   :: SDL.Window
                     , sdlSurface  :: SDL.Surface
                     , sdlRenderer :: SDL.Renderer }
