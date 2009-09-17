-- Some simple functions to generate anagrams of words
import Data.Char
import List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

wordfile = "/usr/share/dict/words"

stringToKey :: String -> String
stringToKey = sort.(map toLower)

validWord :: String -> Bool
validWord s = (not (null s)) && 
              length s <= 10 && 
              not (any (not.isAlpha) s)

anagramList :: String -> IO (Map String (Set String))
anagramList file = do
  filecontent <- readFile file
  return (foldl (\x y -> Map.insertWith Set.union (stringToKey y) (Set.singleton y) x)
                Map.empty 
                (filter validWord $ lines filecontent))

anagramsOf :: String -> IO ()
anagramsOf word = do
  anagrams <- anagramList wordfile
  putStrLn (show (Map.lookup (stringToKey word) anagrams))
