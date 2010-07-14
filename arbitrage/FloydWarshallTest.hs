{-# LANGUAGE MultiParamTypeClasses #-}
module FloydWarshallTest where

import FloydWarshall

import Test.HUnit
import Data.List (nub,(\\))
import Data.Map (Map,keys)
import qualified Data.Map as Map
import Data.Array 
import Data.Maybe (catMaybes)

data Unit = A | B | C | D | E | F | G 
            deriving (Read,Show,Ord,Ix,Eq,Enum)

type ExchangePair = (Unit,Unit)

type ExchangeData = Map ExchangePair Double

data Exchange = Exchange ExchangeData

instance Graph Exchange Unit where
    vertices = vertices'
    edge = edge'
    fromInt = fromInt'

vertices' :: Exchange -> [Unit]
vertices' (Exchange d) = nub $ map snd (keys d)

edge' :: Exchange -> Unit -> Unit -> Maybe Double
edge' (Exchange d) x y = Map.lookup (x,y) d

fromInt' :: Exchange -> Int -> Unit
fromInt' _ = toEnum

basicExchangeData :: Exchange
basicExchangeData = Exchange (Map.fromList d)
    where
      d = [((A,B), 1.2)
          ,((A,C), 0.89)
          ,((B,A), 0.88)
          ,((B,C), 5.10)
          ,((C,A), 1.1)
          ,((C,B), 0.15)]

moreComplex :: Exchange
moreComplex = Exchange (Map.fromList d)
    where
      d = [((A,B), 3.1)
          ,((A,C), 0.0023)
          ,((A,D), 0.35)
          ,((B,A), 0.21)
          ,((B,C), 0.00353)
          ,((B,D), 8.13)
          ,((C,A), 200)
          ,((C,B), 180.559)
          ,((C,D), 10.339)
          ,((D,A), 2.11)
          ,((D,B), 0.089)
          ,((D,C), 0.06111)]

noOpportunity :: Exchange
noOpportunity = Exchange (Map.fromList d)
    where
      d = [((A,B), 2.0), ((B,A), 0.45)]

simpleOpportunity :: Exchange
simpleOpportunity = Exchange (Map.fromList d)
    where
      d = [((A,B), 1.1), ((B,A), 0.95)]

test1 :: Test
test1 = TestCase (do
                   assertEqual "Basic test case 1" (Just [A,B,A]) (findArbitrage basicExchangeData)
                   assertBool "Makes money" (runOpportunity basicExchangeData [A,B,A] > 1.0))

test2 :: Test
test2 = TestCase (do
                   assertEqual "Basic test case 1" (Just [A,B,D,A]) (findArbitrage moreComplex)
                   assertBool "Makes money" (runOpportunity moreComplex [A,B,D,A] > 1.0))                               

test3 :: Test
test3 = TestCase (assertEqual "Basic test case 1" Nothing (findArbitrage noOpportunity))

test4 :: Test
test4 = TestCase (do
                   assertEqual "Basic test case 1" (Just [A,B,A]) (findArbitrage simpleOpportunity)
                   assertBool "Makes money" (runOpportunity simpleOpportunity [A,B,A] > 1.0))

tests :: Test
tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4]

-- If there is an opportunity it should make money!
runOpportunity :: Exchange -> [Unit] -> Double
runOpportunity (Exchange m) x = product $ catMaybes (map (flip Map.lookup m) (zip x (tail x)))


