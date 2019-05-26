module Scene.Editor where

import Scene               (Scene(), mkScene)
import Scene.Editor.Helper (EditorGraph(), EditorData(..), defaultEditorData)
import Scene.Editor.Input  (mkNetwork)
import Scene.Editor.Render (renderEditor)

mkEditor :: EditorGraph -> Scene
mkEditor g = mkEditor' (defaultEditorData {graph = g})
    where
    mkEditor' :: EditorData -> Scene
    mkEditor' ed = mkScene update render
        where
        update = (fmap . fmap . fmap) mkEditor' . mkNetwork ed
        render = renderEditor ed
