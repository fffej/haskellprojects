myadd x y = x + y

mylength [] count = count
mylength (x:xs) count = mylength xs (1 + count)

add5 :: [Int] -> [Int]
add5 [] = []
add5 (x:xs) = (x+5):add5(xs)       

mymap :: (x -> y) -> [x] -> [y]
mymap f [] = []
mymap f (x:xs) = f x:mymap f xs

myfilter :: (x -> Bool) -> [x] -> [x]
myfilter f [] = []
myfilter f (x:xs) | f x = x:myfilter f xs
                  | otherwise =  myfilter f xs