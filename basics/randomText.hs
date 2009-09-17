import Data.Map (Map)
import Data.Foldable
import Data.List (unwords)
import qualified Data.Map as Map

import System.Random

type Followers = Map String Int
type WordSuccessors = Map String Followers

exampleFile :: FilePath
exampleFile = "/home/jfoster/example.txt"

createTrainingSet :: String -> WordSuccessors
createTrainingSet s =  foldl' updateMap Map.empty (wordPairs (words s))

-- If we've seen the word before then we need to +1 to the key
updateMap :: WordSuccessors -> (String,String) -> WordSuccessors
updateMap m (x,y) = Map.insert x v m where
                    q = Map.findWithDefault (Map.singleton y 0) x m
                    v = Map.insert y (succ (Map.findWithDefault 0 y q)) q
    
wordPairs :: [String] -> [(String,String)]
wordPairs l = zip l (tail l)

-- Remember, use "it" for last value in ghci
-- Use :info to print out type defs

-- TODO This algorithm is stupidly bad - the second data structure (Map Int String) is
-- completely wrong.  It should be something many times more efficient
nextWord :: [Int] -> WordSuccessors -> String -> ([Int],String)
nextWord seeds fm start = (r, (poss !! (mod s count))) where 
                          successors = fm Map.! start
                          count = Map.fold (+) 0 successors
                          poss = Map.foldWithKey (\k v acc -> (replicate v k) ++ acc) [] successors
                          s = head seeds
                          r = drop 1 seeds 

maxWordCount :: Int
maxWordCount = 1000000

main :: IO(String)
main = do
  text <- readFile exampleFile
  gen <- newStdGen
  let training = createTrainingSet text
      seeds = randomRs (0,maxWordCount) gen
  return (unwords (map snd (iterate (\(s,w) -> nextWord s training w) (seeds,"by"))))


