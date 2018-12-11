{-# LANGUAGE DuplicateRecordFields #-}

module Test where
--
import Data.List (intercalate)

type Vector2 = (Int, Int)
--

type Index = Int

data Graph = Graph
           { addNode :: Vector2 -> Graph
           , cursors :: [Node] }

data Node = Node
          { ix       :: Index
          , position :: Vector2
          , pins     :: [Pin] }
data Pin = Pin
         { ix        :: Index
         , pinType   :: PinType
         , parent    :: Node
         , otherPins :: [Pin] }
data PinType = InputPin | OutputPin deriving (Eq)

mkGraph :: Graph
mkGraph =
    let newGraph = Graph (addNode' newGraph 0) []
        addNode' g ix p =
            let g' = Graph (addNode' g' (ix + 1)) (mkNode ix p : cursors g)
            in  g'
    in  newGraph

mkNode :: Int -> Vector2 -> Node
mkNode ix p = Node ix p []

instance Show Graph where
    show (Graph _ cursors') = "Graph count: " ++ show (length cursors') ++ "\n" ++ intercalate "\n" (fmap show cursors')

instance Show Node where
    show (Node ix' position' pins') = "Node " ++ show ix' ++ " - pin count: " ++ show (length pins')

--

data TestNode = TestNode
              { val :: Int
              , prev :: TestNode
              , next :: TestNode }

instance Show TestNode where
    show (TestNode val' prev' next') =
        show (val (prev prev')) ++ " " ++
        show (val prev') ++
        " [" ++ show val' ++ "] " ++
        show (val next') ++ " " ++
        show (val (next next'))

mkTest :: TestNode
mkTest = let testNode = TestNode 1 prevNode nextNode
             prevNode = TestNode 0 prevNode testNode
             nextNode = TestNode 2 testNode nextNode
         in  testNode

stepPrev :: TestNode -> TestNode
stepPrev (TestNode _ p _) = p

stepNext :: TestNode -> TestNode
stepNext (TestNode _ _ n) = n

changeNext :: TestNode -> TestNode -> TestNode
changeNext newNext (TestNode v p _) = TestNode v p newNext

changePrev :: TestNode -> TestNode -> TestNode
changePrev newPrev (TestNode v _ n) = TestNode v newPrev n

changeVal :: Int -> TestNode -> TestNode
changeVal v (TestNode _ p n) =
    let newNode = TestNode v (changeNext newNode p) (changePrev newNode n)
    in  newNode