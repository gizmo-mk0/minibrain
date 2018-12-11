{-# LANGUAGE DuplicateRecordFields #-}

module Test where
--
import Data.List (intercalate)
import Control.Monad.Fix (fix)

type Vector2 = (Int, Int)
--

type Index = Int

data Graph = MkGraph
           { addNode :: Vector2 -> Graph
           , cursors :: [Node] }

data Node = MkNode
          { ix       :: Index
          , position :: Vector2
          , pins     :: [Pin] }

data Pin = MkPin
         { ix        :: Index
         , pinType   :: PinType
         , parent    :: Node
         , otherPins :: [Pin] }
         
data PinType = InputPin | OutputPin deriving (Eq)

mkGraph :: Graph
mkGraph = fix $ \graph -> MkGraph (addNode' graph 0) []
    where
    addNode' g ix p = fix $ \g' -> MkGraph (addNode' g' (ix + 1)) (mkNode ix p : cursors g)


mkNode :: Index -> Vector2 -> Node
mkNode ix p = MkNode ix p []

instance Show Graph where
    show (MkGraph _ cursors') = "Graph count: " ++ show (length cursors') ++ "\n" ++ intercalate "\n" (fmap show cursors')

instance Show Node where
    show (MkNode ix' position' pins') = "Node " ++ show ix' ++ " - pin count: " ++ show (length pins')

--

data TestNode = MkTestNode
              { val :: Int
              , prev :: TestNode
              , next :: TestNode }

instance Show TestNode where
    show (MkTestNode val' prev' next') =
        show (val (prev prev')) ++ " " ++
        show (val prev') ++
        " [" ++ show val' ++ "] " ++
        show (val next') ++ " " ++
        show (val (next next'))

mkTest :: TestNode
mkTest = let testNode = MkTestNode 1 prevNode nextNode
             prevNode = MkTestNode 0 prevNode testNode
             nextNode = MkTestNode 2 testNode nextNode
         in  testNode

stepPrev :: TestNode -> TestNode
stepPrev (MkTestNode _ p _) = p

stepNext :: TestNode -> TestNode
stepNext (MkTestNode _ _ n) = n

changeNext :: TestNode -> TestNode -> TestNode
changeNext newNext (MkTestNode v p _) = MkTestNode v p newNext

changePrev :: TestNode -> TestNode -> TestNode
changePrev newPrev (MkTestNode v _ n) = MkTestNode v newPrev n

changeVal :: Int -> TestNode -> TestNode
changeVal v (MkTestNode _ p n) =
    let newNode = MkTestNode v (changeNext newNode p) (changePrev newNode n)
    in  newNode