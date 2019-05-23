module Scene.Editor where

import Data.Tuple (swap)

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
        -- originally, this line looked like this:
        -- update = fmap (fmap (fmap mkEditor')) . mkNetwork ed
        -- But after reading this: https://www.reddit.com/r/haskell/comments/6qooky/fmap_fmap_fmap_fmap_fmap_fmap_fmap_fmap/
        -- I decided that I want to have fun. So here it is:
        update = fmap fmap fmap fmap fmap fmap fmap fmap mkEditor'
               . mkNetwork ed
        render = renderEditor ed

-- \(newEd, cmd) -> (mkEditor' newEd, cmd)
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fmap . fmap ::
-- ((a -> b) -> (f a -> f b)) -> ()