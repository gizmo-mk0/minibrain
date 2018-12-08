module Render where

--
import Types

renderCurrentScene :: SceneData -> Minibrain ()
renderCurrentScene scene =
    case currentScene scene of
        Title -> renderTitle scene
        Briefing -> renderBriefing scene
        Editor -> renderEditor scene
        Simulation -> renderSimulation scene
        Quit -> return ()

renderTitle :: SceneData -> Minibrain ()
renderTitle = undefined

renderBriefing :: SceneData -> Minibrain ()
renderBriefing = undefined

renderEditor :: SceneData -> Minibrain ()
renderEditor = undefined

renderSimulation :: SceneData -> Minibrain ()
renderSimulation = undefined