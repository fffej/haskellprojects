module Stemming (parseRules) where

import qualified Data.Map as Map
import Data.Char (isLower)
import Data.List (isSuffixOf,find,(!!))
import System (getArgs)
import Data.Maybe (fromJust)
import Test.HUnit
import Text.ParserCombinators.Parsec

data RuleType = Stop
              | Continue
                deriving (Show,Eq)

data Rule = Rule {
      suffix :: String
    , drop :: Int
    , intact :: Bool
    , action :: RuleType
} deriving (Show,Eq)

parseRules :: GenParser Char st [Rule]
parseRules = do
  result <- many parseRule
  eof
  return result

parseRule :: GenParser Char st Rule
parseRule = do
  suffix <- many1 letter
  skipCount <- many1 digit
  -- TODO MIGHT be a * here.
  action <- oneOf ">."
  _ <- oneOf "\n"
  let ruleType = if action == '>'
                 then Continue
                 else Stop
  return (Rule suffix (read skipCount) False ruleType)

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

isConsonant :: Char -> Bool
isConsonant s = isLower s && not (isVowel s)

-- |If the stem begins with a vowel, then it must contain at least 2 letters,
-- |one of which must be a consonant.  If however, it beings with a vowel then
-- |it must contain three letters and at least one of these must be a vowel
-- |or a 'y'
isAcceptable :: String -> Bool
isAcceptable s = len > 3 && 
               (fstIsVowel && sndIsConsonant) &&
               (sndIsVowel || thirdIsVowel)
    where
      len = length s
      fstIsVowel = isVowel $ head s
      sndChar = s !! 1
      sndIsVowel = len >= 2 && isVowel sndChar
      sndIsConsonant = len >= 2 && isConsonant sndChar
      thirdIsVowel = len >= 3 && isVowel (s !! 2)

firstMatchRule :: String -> [Rule] -> Maybe Rule
firstMatchRule word rules = find (ruleMatches word) rules

ruleMatches :: String -> Rule -> Bool
ruleMatches word (Rule suffix _ _ _) = suffix `isSuffixOf` word

stem :: String -> [Rule] -> String
stem word [] = word
stem word rules | Nothing == matchingRule = word
                | otherwise = undefined
    where
      matchingRule = firstMatchRule word rules
      (Rule suffix drop intact action) = fromJust matchingRule
  _ <- many (noneOf "\n")
  return (Rule keyString replaceString ruleType)

-- TODO tomorrow actually compile

{-
main = do
  args <- getArgs
  let inputFile = head args
      outputFile = args !! 1
  rulesString <- readFile inputFile
  let (Right rules) = parseRules rulesString
  print rules
  return ()
-}