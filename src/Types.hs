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

data GameScene       = Menu MenuData
                     | Briefing BriefingData
                     | Editor EditorData
                     | Simulation SimulationData
                     | Quit
data MenuData        = MenuData
data BriefingData    = BriefingData
data EditorData      = EditorData
                     { perceptrons :: [Perceptron] }
data SimulationData  = SimulationData
data GameData        = GameData GameScene
data SdlData         = SdlData SdlGraphicsData
data SdlGraphicsData = SdlGraphicsData
                     { sdlWindow   :: SDL.Window
                     , sdlSurface  :: SDL.Surface
                     , sdlRenderer :: SDL.Renderer }

instance Eq GameScene where
    Menu       _ == Menu       _ = True
    Briefing   _ == Briefing   _ = True
    Editor     _ == Editor     _ = True
    Simulation _ == Simulation _ = True
    Quit         == Quit         = True
    _            == _            = False
