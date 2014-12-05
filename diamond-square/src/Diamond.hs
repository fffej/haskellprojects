module Diamond where

data QuadTree = QuadTree QuadTree QuadTree QuadTree QuadTree
              | Leaf Int
              deriving (Show)

render :: QuadTree -> [Int]
render (Leaf n) = [n]
render (QuadTree a b c d) = render a ++ render b ++ render c ++ render d

{-
  12
  34
-}
twoByTwo :: QuadTree
twoByTwo = QuadTree (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 4)


{-
  1212
  3434
  1212
  3434
   QuadTree 
-}

fourByFour :: QuadTree
fourByFour = QuadTree twoByTwo twoByTwo twoByTwo twoByTwo
