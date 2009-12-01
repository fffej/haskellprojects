import Data.Maybe

-- fmap :: (a -> b) -> f a -> f b
onList = fmap (+ 1) [1,2,3]
onJust = fmap (+ 1) (Just 4)
onNothing = fmap (+ 1) Nothing

data MyTree a = Leaf a
              | Node a (MyTree a) (MyTree a)
                deriving (Show,Eq)

instance Functor MyTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a b c) = (Node (f a) (fmap f b) (fmap f c))

exampleTree :: MyTree Int
exampleTree = Node 5 (Node 3 (Leaf 2) (Leaf 1)) (Node 6 (Leaf 7) (Leaf 8))

