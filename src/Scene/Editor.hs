module Scene.Editor where

import Control.Varying.Core  (Var)
import Control.Varying.Event (Event)

import Input (InputEvent)

import Scene               (Scene)
import Scene.Editor.Helper (EditorGraph, EditorData(..), defaultEditorData)
import Scene.Editor.Input  (mkNetwork)
import Scene.Editor.Render (renderEditor)

mkEditor :: EditorGraph -> Var (Event InputEvent) Scene
mkEditor g = mkEditor' $ defaultEditorData {graph = g}
    where
    mkEditor' :: EditorData -> Var (Event InputEvent) Scene
    mkEditor' ed = mkNetwork ed
