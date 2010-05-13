module Stemming (main,parseRules) where

import qualified Data.Map as Map
import Data.Char (isLower)
import Data.List (isSuffixOf)
import System (getArgs)
import System.Console.GetOpt


import Text.ParserCombinators.Parsec

data RuleType = Protect
              | Intact
              | Continue
              | ContInt
              | ProtInt
              | Stop 
                deriving (Show)
        
data Rule = Rule {
      keyString :: String
    , replaceString :: String
    , ruleType :: RuleType
} deriving (Show)

data RuleResult = RuleStop | RuleContinue | RuleNotApplied deriving (Eq)

type Rules = Map.Map Char [Rule]

makeRulesTable :: [Rule] -> Rules
makeRulesTable = foldl insertRule Map.empty where
    insertRule mp r@(Rule k _ _) = Map.insertWith (++) (last k) [r] mp

rules :: GenParser Char st Rules
rules = do
  result <- many rule
  eof
  return (makeRulesTable result)

rule :: GenParser Char st Rule
rule = do
  keyString <- many (noneOf ",")
  char ','
  replaceString <- many (noneOf (","))
  char ','
  ruleType <- parseRuleType
  many (noneOf "\n")
  return (Rule keyString replaceString ruleType)

-- |http://legacy.cs.uu.nl/daan/download/parsec/parsec.html 
-- See the section of consuming input for the reasoning on try
parseRuleType :: GenParser Char st RuleType
parseRuleType = do
  (try $ string "intact" >> return Intact)
  <|> (try $ string "continue" >> return Continue)
  <|> (try $ string "contint" >> return ContInt)
  <|> (try $ string "protect" >> return Protect)
  <|> (try $ string "protint" >> return ProtInt)
  <|> (try $ string "stop" >> return Stop)

parseRules :: String -> Either ParseError Rules
parseRules input = parse rules "(unknown)" input

continue :: RuleType -> Bool
continue Continue = True
continue ContInt = True
continue _ = False

protect :: RuleType -> Bool
protect Protect = True
protect ProtInt = True
protect _ = False

intact :: RuleType -> Bool
intact Intact = True
intact ProtInt = True
intact ContInt = True

isVowel :: Char -> Bool
isVowel = flip elem "aeiou"

isConsonant :: Char -> Bool
isConsonant s = isLower s && not (isVowel s)

-- |If the stem begins with a vowel, then it must contain at least 2 letters,
-- |one of which must be a consonant.  If however, it beings with a vowel then
-- |it must contain three letters and at least one of these must be a vowel
-- |or a 'y'
acceptable :: String -> Bool
acceptable s = len > 3 && 
               (fstIsVowel && sndIsConsonant) &&
               (sndIsVowel || thirdIsVowel)
    where
      len = length s
      fstIsVowel = isVowel $ head s
      sndChar = s !! 1
      sndIsVowel = len >= 2 && isVowel sndChar
      sndIsConsonant = len >= 2 && isConsonant sndChar
      thirdIsVowel = len >= 3 && isVowel (s !! 2)

-- |Apply the rule giving the result together with the stemmed word
applyRule :: Rule -> String -> Bool -> (String,RuleResult)
applyRule r@(Rule keyString replaceString ruleType) word itct | shouldBeIntact || (not endingMatches) = (word,RuleNotApplied)
                                                              | (protect ruleType) = (word,RuleStop)
                                                              | continue ruleType = (replaceString,RuleContinue)
                                                              | otherwise = (replaceString,RuleStop)
    where
      shouldBeIntact = itct && (not $ intact ruleType)
      endingMatches = keyString `isSuffixOf` word

applyRules :: Rules -> String -> String
applyRules r s = undefined where
    rules = Map.findWithDefault [] (last s) r

main = do
  args <- getArgs
  let inputFile = args !! 0
      outputFile = args !! 1
  rulesString <- readFile inputFile
  let (Right rules) = parseRules rulesString
  print rules
  return ()