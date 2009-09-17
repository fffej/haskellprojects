import qualified Data.Map as Map
import Test.BenchPress
import System.Random
import Maybe

alist :: [(String,Double)]
alist = [("pi", 3.14159265), ("e", 2.71828183), ("phi", 1.61803398874)]

getConstant :: String -> Maybe Double
getConstant name = lookup name alist

listSize = 10000;

bigList :: Integer -> [(Integer,Integer)]
bigList n = map (\x -> (x, x*2)) [1..n]

randomLookup l =  do
  r1 <- getStdGen
  let (x, r2) = randomR (0,listSize) r1
  setStdGen r2
  return (lookup x l)

timeLookups :: IO ()
timeLookups = let exampleList = (bigList listSize) in   
                bench 100 $ do
                  a <- randomLookup (bigList listSize)
                  putStr (show a)
                  return ()

-- Association lists are O(N) lookup via a linear scan
-- Maps are O(1) lookup

mlist :: Map.Map String Double 
mlist = Map.fromList alist

mlist2 :: Map.Map String Double
mlist2 = Map.insert "pi" 3.14159265 $ Map.empty