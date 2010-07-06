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

import FloydWarshall

data Currency = AUD
              | CAD
              | CHF
              | JPY
              | USD
              | EUR
              | DKK
              | GBP
              | NOK
              | NZD
              | SEK
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

{-

I grabbed the data from http://ratedata.gaincapital.com/ and used wget -r "http://ratedata.gaincapital.com/2010/01%20January/"

You can bung it all together with cat *.csv > foo

Once you've got the data you can "sort -k1,1 -n foo" to group it. 

tail -n +XYZ foo.csv discards the first XYZ lines of a file.

-}

-- The weighted graph
data Exchange = Exchange {
      vertices :: [Currency]
    }

{- 

Floyd-Warshall graph Algorithm.

  Vertices are the currencies

        BUY        SELL
  GBP ------> USD ------> GBP

  GBP ---> USD ---> EUR ---> GBP

  Construct a graph where each vertex is a nation and there is an edge weighted lg(w(x,y)) from x to y if the exchange rate from currency x to currency y is w(x,y).  In arbitrage, we seek a cycle to convert currencies so that we end up with more money than we started with.

-}
{-
main = do
  a <- parseFile "/home/jeff/workspace/Haskell/haskellprojects/arbitrage/data/sorted_data.csv"
  print (length a)
-}
  
{-

<a href="http://en.wikipedia.org/wiki/Arbitrage">Arbitrage</a> is <quote>the practice of taking advantage of a price difference between two or more markets, striking a combination of matching deals that capitalize upon the imbalance, the profit being the difference between the market prices.  

TODO word better, check matches - see http://en.wikipedia.org/wiki/Fixed-odds_betting
A simple example is a tennis match between two evenly matched players.  One bookie might offer odds of 11/10 for one player, and another 11/10 for the other player.  Putting $10 on each player means that you're guaranteed to win one bet and thus come out on top ($20 down, winning will yield you $22 yielding a profit of $2).

Arbitrage situations shouldn't exist in an efficient market, but the <i>arbitrage paradox</i> (Grossman and Stiglitz) says that if arbitrage is never observed, market participants may not have sufficient incentives to watch the market, in which case arbitrage opportunities could arise.  One resolution to this paradox is that opportunities do exist, though they are very short lived.

-}

data ForexExchange = ForexExchange

instance Graph ForexExchange Currency where
    vertices = vertices'
    edge = edge'
    fromInt = fromInt'

vertices' :: ForexExchange -> [Vertex Currency]
vertices' _ = map Vertex [AUD .. SEK]

edge' :: ForexExchange -> Vertex a -> Vertex a -> Maybe Double
edge' = undefined

fromInt' :: ForexExchange -> Int -> Vertex a
fromInt' = undefined 

main = do
  print "Hello world."