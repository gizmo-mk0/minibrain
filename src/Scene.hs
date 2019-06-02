{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Scene where

import qualified SDL

import Control.Event.Handler (AddHandler)
import Data.IORef            (IORef, modifyIORef)
import Data.Maybe            (fromJust)
import Control.Varying.Core  (Var, arr)
import Control.Varying.Event (Event)

import Input  (InputEvent(..))
import Types  (Vector2f)
import Render (VectorImage, blank)
import Input  (InputEvent(..))

import Utils

data Scene = Scene
           { cmd    :: StackCommand
           , render :: VectorImage }

type SceneArr = Var InputEvent Scene

-- mkScene :: a -> Var InputEvent (StackCommand, Scene) -> VectorImage -> Scene
-- mkScene sceneData = mkScene' sceneData updateScene renderScene
--     where
--     mkScene' sd upd = Scene updateF renderF
--         where
--         (newSceneData, newUpd) = runIdentity $ runVarT upd sd
--         update = mkScene' newSceneData newUpd

emptyScene :: Var (Event InputEvent) Scene
emptyScene = arr $ const $ Scene None blank

data StackCommand = None          -- No scene change
                  | Done          -- Current scene is done, remove from stack
                  | Push (Var (Event InputEvent) Scene)    -- Add scene to the stack
                  | Replace (Var (Event InputEvent) Scene) -- Replace current scene with another scene
                --   | Quit          -- Remove all scenes from stack

isNone :: StackCommand -> Bool
isNone None = True
isNone _    = False

-- | Update the top state on the stack
updateStack :: StackCommand -> Stack (Var (Event InputEvent) Scene)
            -> Stack (Var (Event InputEvent) Scene)
updateStack cmd stack =
    case cmd of
        None      -> stack
        Done      -> fromJust $ snd (pop stack)
        Replace s -> replace s stack
        Push s    -> push s stack