module StableMarriage where

import Data.List
import Data.Maybe

stableMatch :: (Eq m, Eq w) => [(m,[w])] -> [(w,[m])] -> [(m,w)]
stableMatch ms ws = stableMatch' []
  where       
    stableMatch' ps = case unmarried ms ps of
      Just unmarriedMan  -> stableMatch' (findMatch unmarriedMan ws ps)
      Nothing            -> ps
                        
-- Outcome - m is always married to someone 
findMatch :: (Eq m,Eq w) => (m,[w]) -> [(w,[m])] -> [(m,w)] -> [(m,w)]
findMatch (m,w:rest) ws ps = case isEngaged w ps of
      
  -- w is already engaged to m' - is there a better match?
  Just m' -> if prefers (getPrefs ws w) m m'
             then engage (breakup m' ps) m w
             else findMatch (m,rest) ws ps
                      
  -- can match with first choice
  Nothing -> engage ps m w
                       
getPrefs :: Eq w => [(w,m)] -> w -> m
getPrefs ws w = fromJust $ lookup w ws
                         
isEngaged :: Eq w => w -> [(m,w)] -> Maybe m
isEngaged w ps = fmap fst (find (\x -> snd x == w) ps)
                         
engage :: [(m,w)] -> m -> w -> [(m,w)]
engage xs a b = (a,b) : xs

breakup :: Eq m => m -> [(m,w)] -> [(m,w)]
breakup m = filter (\x -> fst x /= m)                         

-- Returns the first man in in ms not in ps
unmarried :: Eq m => [(m,[w])] -> [(m,w)] -> Maybe (m,[w])
unmarried ms ps = find (\(m,_) -> m `notElem` engagedMen) ms
  where
    engagedMen = map fst ps

-- Returns true if w prefers first over second
prefers :: Eq m => [m] -> m -> m -> Bool
prefers ms m1 m2 = go ms 
  where
    go []     = error "no match" 
    go (x:xs)
      | x == m1   = True
      | x == m2   = False
      | otherwise = go xs
    
