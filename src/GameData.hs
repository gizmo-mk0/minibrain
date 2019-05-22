-- generic-lens
-- {-# LANGUAGE AllowAmbiguousTypes       #-}
-- {-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE DeriveGeneric             #-}
-- {-# LANGUAGE DuplicateRecordFields     #-}
-- {-# LANGUAGE FlexibleContexts          #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE TypeApplications          #-}

module GameData where

import qualified SDL
import qualified NanoVG as NVG
import qualified Linear as L
import Control.Lens ((^.), (.~), (&))
-- import Data.Generics.Product.Fields (field)

import Reactive.Banana.Combinators

import GHC.Generics (Generic)

import Types
import Input

data Config = Config
            { getWindow     :: SDL.Window
            , getWindowSize :: Vector2i
            , getGlContext  :: SDL.GLContext
            , getNvgContext :: NVG.Context }

data GameData = GameData
              { eventData :: Event InputEvent
              , mousePosB :: Behavior Vector2f}
            --   deriving (Generic)

data CameraData = CameraData
                { cPosition   :: Vector2f
                , cRotation   :: Float
                , cZoom       :: Float }
                deriving (Show)

-- data TimeData = TimeData
--               { currentTime    :: Float
--               , sinceLastFrame :: Float }

-- cameradata windowSize position
toWorldCoords :: CameraData -> Vector2i -> Vector2f -> Vector2f
toWorldCoords (CameraData (SDL.V2 cx cy) r z) (SDL.V2 w h) (SDL.V2 x y) =
    let (L.V2 lx ly) = (^. L._xy)
                     -- applying camera transformation in reverse:
                     . L.rotate (L.axisAngle (L.V3 0 0 1) (pi * r / 180))
                     . (/ (L.V3 z z z))
                     . (+ (L.V3 (-cx) (-cy) 0))
                     $ (L.V3 x y 0)
    in  SDL.V2 lx ly

-- changeScene :: Scene -> GameData -> GameData
-- changeScene s gd = gd {sceneData = (sceneData gd) {currentScene = s}}

-- updateSelectionRect :: GameData -> Vector2f -> GameData
-- updateSelectionRect gd p =
--     gd & field @"sceneData"
--        . field @"editorData"
--        . field @"selectionRect"
--        .~ Just (Rect2f (SDL.V2 0 0) p)

