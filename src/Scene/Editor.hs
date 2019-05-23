module Scene.Editor where

import Scene               (Scene(), mkScene)
import Scene.Editor.Helper (EditorGraph(), EditorData(..), defaultEditorData)
import Scene.Editor.Input  (mkNetwork)
import Scene.Editor.Render (renderEditor)
import GameData            (getWindowSize)

mkEditor :: EditorGraph -> Scene
mkEditor g = mkEditor' (defaultEditorData {graph = g})

mkEditor' :: EditorData -> Scene
mkEditor' ed = mkScene update render
    where
    update =
        \gd -> fmap (fmap (\(newEd, cmd) -> (mkEditor' newEd, cmd))) (mkNetwork ed gd)
    render cfg = renderEditor (getWindowSize cfg) ed