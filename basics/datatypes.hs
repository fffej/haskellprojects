-- Goal to investigate some data types

-- This is called a product type
data Point = Point { x :: Float
                   , y :: Float } deriving (Show, Eq)

{--
Main> Point 3.0 4.0 == Point 1.0 2.0

<interactive>:1:0:
    No instance for (Eq Point)
      arising from a use of `==' at <interactive>:1:0-29
    Possible fix: add an instance declaration for (Eq Point)
    In the expression: Point 3.0 4.0 == Point 1.0 2.0
    In the definition of `it': it = Point 3.0 4.0 == Point 1.0 2.0
--}


