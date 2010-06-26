module Forex where

import Text.ParserCombinators.Parsec

import Control.Monad (liftM,liftM2,liftM5)

import Data.Time.Clock
import Data.Time.Format (parseTime)
import Data.Maybe

import System.Locale (defaultTimeLocale)

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
                deriving (Read,Show)

type CurrencyPair = (Currency,Currency)

data ForexEntry = ForexEntry {
      lTid :: Integer
    , currencyPair :: CurrencyPair
    , rateDateTime :: UTCTime
    , rateBid :: Double
    , rateAsk :: Double
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
currencyParse = do
  s <- many (noneOf "/,\n")
  return (read s)

-- 1029209135,D,AUD/CAD,2010-01-03 17:03:04,.944900,.945800
entry :: GenParser Char st ForexEntry
entry = liftM5 ForexEntry parseInteger
                          (string ",D," >> currencyPairParse)
                          (char ',' >> timeParser)
                          (char ',' >> parseDouble)
                          (char ',' >> parseDouble)

parseInteger :: GenParser Char st Integer
parseInteger = liftM read cell

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
  c <- readFile s
  case (parse forexHistory "Failed" c) of
    Left _ -> error "Failed to parse"
    Right q -> return q
  

{-

<a href="http://en.wikipedia.org/wiki/Arbitrage">Arbitrage</a> is <quote>the practice of taking advantage of a price difference between two or more markets, striking a combination of matching deals that capitalize upon the imbalance, the profit being the difference between the market prices.  

TODO word better, check matches - see http://en.wikipedia.org/wiki/Fixed-odds_betting
A simple example is a tennis match between two evenly matched players.  One bookie might offer odds of 11/10 for one player, and another 11/10 for the other player.  Putting $10 on each player means that you're guaranteed to win one bet and thus come out on top ($20 down, winning will yield you $22 yielding a profit of $2).

Arbitrage situations shouldn't exist in an efficient market, but the <i>arbitrage paradox</i> (Grossman and Stiglitz) says that if arbitrage is never observed, market participants may not have sufficient incentives to watch the market, in which case arbitrage opportunities could arise.  One resolution to this paradox is that opportunities do exist, though they are very short lived.

-}
