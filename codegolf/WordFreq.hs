import Data.Map (Map)
import qualified Data.Map as M
import Data.List (sortBy)

ignoredWords = words "the and of to a i it in or is"

countInMap m k = M.insertWith (+) k 1 m

countWords s = take 22 $ sortList $ foldl countInMap M.empty w
    where 
      w = filter (not . (`elem` ignoredWords)) (words s)

sortList m = sortBy (\(_,x) (_,y) -> compare y x) (M.toList m)

maxLength = snd . head

draw w = (' ' : h ++ concatMap (drawItem ww) w)
    where
      n = (snd . head) w -- max occurences of the term 
      lw = foldl1 max (map (length . fst) w) -- longest word
      wi = 80 - (lw + 3) -- wi is the number of characters we have to fill
      ww = ((realToFrac wi) / (realToFrac n)) :: Double -- width of each occurence should be max occurences
      h = take (round (ww * realToFrac n)) (repeat '_') ++ "\n"

drawItem ww (w,n) = "|" ++ (take x (repeat '_')) ++ "| " ++ w ++ "\n"
    where
      x = round ((realToFrac n) * ww)

main = interact (draw . countWords)