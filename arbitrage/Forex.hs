{-# LANGUAGE MultiParamTypeClasses #-}
module Forex (main) where

import Text.ParserCombinators.Parsec (endBy,sepBy,char,many,noneOf,string,parse)
import Text.Parsec.ByteString.Lazy
import Control.Monad (liftM,liftM2,liftM5)
import Data.Time.Clock
import Data.Time.Format (parseTime)
import Data.Maybe
import Data.Ix
import qualified Data.ByteString.Lazy as B
import System.Locale (defaultTimeLocale)

import Data.Map (Map,keys)
import qualified Data.Map as Map
import Data.List (nub)

import FloydWarshall

data Currency = AUD | GBP | USD | CAD | CHF | JPY | EUR | DKK 
              | NOK | NZD | SEK | SGD | HKD | XAU | XAG
                deriving (Read,Show,Ord,Ix,Eq,Enum)

type CurrencyPair = (Currency,Currency)

data ForexEntry = ForexEntry {
      lTid :: Int
    , currencyPair :: CurrencyPair
    , rateDateTime :: UTCTime
    , rateBid :: Double  -- Price to the buyer
    , rateAsk :: Double  -- Price to the seller
    } deriving Show

forexHistory :: GenParser Char st [ForexEntry]
forexHistory = header >> eol >> endBy entry eol

header :: GenParser Char st [String]
header = sepBy cell (char ',')

eol :: GenParser Char st Char
eol = char '\n'

cell :: GenParser Char st String
cell = many (noneOf ",\n")

currencyPairParse :: GenParser Char st CurrencyPair
currencyPairParse = liftM2 (,) currencyParse (char '/' >> currencyParse)

currencyParse :: GenParser Char st Currency
currencyParse = liftM read (many (noneOf "/,\n"))

entry :: GenParser Char st ForexEntry
entry = liftM5 ForexEntry parseInt
                          (string ",D," >> currencyPairParse)
                          (char ',' >> timeParser)
                          (char ',' >> parseDouble)
                          (char ',' >> parseDouble)

parseInt :: GenParser Char st Int
parseInt = liftM read cell

parseDouble :: GenParser Char st Double
parseDouble = liftM readDouble cell

readDouble :: String -> Double
readDouble s = read x 
    where
      x | head s == '.' = '0':s
        | otherwise = s

timeParser :: GenParser Char st UTCTime
timeParser = liftM readTime (many (noneOf ","))

readTime :: String -> UTCTime
readTime s | x == Nothing = error ("Undefined date format for " ++ s)
           | otherwise = fromJust x
    where
      x = parseTime defaultTimeLocale "%F %T" s

parseFile :: FilePath -> IO [ForexEntry]
parseFile s = do
  putStrLn $ "Reading " ++ s
  c <- B.readFile s
  case (parse forexHistory "Failed" c) of
    Left _ -> error "Failed to parse"
    Right q -> return q

type ExchangePair = (Currency,Currency)
type ExchangeData = Map ExchangePair Double
data Exchange = Exchange ExchangeData

instance Graph Exchange Currency where
    vertices = vertices'
    edge = edge'
    fromInt = fromInt'

instance Show Exchange where
    show (Exchange s) = show s

vertices' :: Exchange -> [Currency]
vertices' (Exchange d) = nub $ map snd (keys d)

edge' :: Exchange -> Currency -> Currency -> Maybe Double
edge' (Exchange d) x y = Map.lookup (x,y) d

fromInt' :: Exchange -> Int -> Currency
fromInt' _ = toEnum

update :: Exchange -> ForexEntry -> Exchange
update (Exchange m) f = Exchange (Map.insert (b,a) sell 
                                  (Map.insert (a,b) (1 / buy) m))
    where
      (a,b) = currencyPair f
      buy = rateBid f
      sell = rateAsk f

parseRecs :: [ForexEntry] -> [(Maybe [Currency], Exchange)]
parseRecs recs = v
    where
      exchanges = scanl update (Exchange Map.empty) recs
      opportunities = zip (map findArbitrage exchanges) exchanges
      v = filter (\(x,y) -> x /= Nothing) opportunities

main = do
  recs <- parseFile "/home/jeff/workspace/Haskell/haskellprojects/arbitrage/data/small_sorted_set.csv"
  print (take 10 $ parseRecs recs)
{-

<a href="http://en.wikipedia.org/wiki/Arbitrage">Arbitrage</a> is <quote>the practice of taking advantage of a price difference between two or more markets, striking a combination of matching deals that capitalize upon the imbalance, the profit being the difference between the market prices.  

TODO word better, check matches - see http://en.wikipedia.org/wiki/Fixed-odds_betting
A simple example is a tennis match between two evenly matched players.  One bookie might offer odds of 11/10 for one player, and another 11/10 for the other player.  Putting $10 on each player means that you're guaranteed to win one bet and thus come out on top ($20 down, winning will yield you $22 yielding a profit of $2).

Arbitrage situations shouldn't exist in an efficient market, but the <i>arbitrage paradox</i> (Grossman and Stiglitz) says that if arbitrage is never observed, market participants may not have sufficient incentives to watch the market, in which case arbitrage opportunities could arise.  One resolution to this paradox is that opportunities do exist, though they are very short lived.

-}

  
  