-- Gather information from a log file in a functional way.
import Data.Time.Clock
import Data.Time.Calendar
import Data.List
import Data.Time.Format
import Maybe
import System.Locale
import Char

import Data.Map (Map)
import qualified Data.Map as Map

{-- 

Rejected this approach after reading these two articles

* http://www.haskell.org/haskellwiki/OOP_vs_type_classes
* http://www.ninebynine.org/Software/Learning-Haskell-Notes.html

It's probably the wrong way to go

class Report2 r where
    printReport :: r -> String

class (Eq l, Show l) => LogProcessor l where
    processLine2 :: String -> Maybe l
    combineUnit :: (Report2 r) => l -> r -> r

processFile2 :: LogProcessor(a) => FilePath -> a -> IO([a])
processFile2 s l = do 
  a <- readFile s
  return (Maybe.mapMaybe processLine2 (lines a))
--}


timeFormat :: String
timeFormat = "%F %T"

type Package = String

data Upgrade = Upgrade { packageName :: Package
                       , updateTime :: UTCTime } 

instance Show Upgrade where
    show a = show (updateTime a) ++ 
             ":" ++ show (packageName a) ++ "\n"

getTime :: String -> UTCTime
getTime = fromJust . parseTime defaultTimeLocale timeFormat

getPackageName :: String -> String
getPackageName = takeWhile (not . Char.isSpace)

parseLine :: String -> Maybe Upgrade
parseLine s 
          | isInfixOf " upgrade " s = Just 
                                      (Upgrade 
                                       (takeWhile (not . Char.isSpace) (drop 28 s))
                                       (getTime (take 20 s)))
          | otherwise = Nothing


processFile :: FilePath -> (String -> Maybe t) -> IO([t])
processFile path f = do
  a <- readFile path
  return (Maybe.mapMaybe f (lines a))

type Report = Map Day [Package]

combine :: [Upgrade] -> String
combine = show . foldl addToReport Map.empty

addToReport :: Report -> Upgrade -> Report
addToReport r p = Map.insert day packages r where
                  day = utctDay (updateTime p)
                  initVal = Map.findWithDefault [] day r
                  packages = packageName p:initVal

reportFile :: FilePath -> (String -> Maybe t) -> ([t] -> String) -> IO()
reportFile path func comb  = do
  a <- processFile path func
  print (comb a)
  return ()




{--
exampleFile :: String
exampleFile = "/home/jfoster/package_logs.txt"
 
timeFormat :: String
timeFormat = "%F %T"

type Package = String

data Upgrade = Upgrade { packageName :: Package
                       , updateTime :: UTCTime } 

instance Show Upgrade where
    show a = show (updateTime a) ++ 
             ":" ++ show (packageName a) ++ "\n"

getTime :: String -> UTCTime
getTime = fromJust . parseTime defaultTimeLocale timeFormat

getPackageName :: String -> String
getPackageName = takeWhile (not . Char.isSpace)
               
-- Poor mans parsing.
parseLine :: String -> Maybe Upgrade
parseLine s 
          | isInfixOf " upgrade " s = Just 
                                      (Upgrade 
                                       (takeWhile (not . Char.isSpace) (drop 28 s))
                                       (getTime (take 20 s)))
          | otherwise = Nothing

processFile :: FilePath -> IO([Upgrade])
processFile s = do 
  a <- readFile s
  return (Maybe.mapMaybe parseLine (lines a))

type Report = Map Day [Package]

combine :: [Upgrade] -> Report
combine = foldl addToReport Map.empty 

addToReport :: Report -> Upgrade -> Report
addToReport r p = Map.insert day packages r where
                  day = utctDay (updateTime p)
                  initVal = Map.findWithDefault [] day r
                  packages = packageName p:initVal

reportFile :: FilePath -> IO()
reportFile f = do
  a <- processFile f
  print (combine a)
  return ()
--}