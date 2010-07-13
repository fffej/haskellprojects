{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshallTest where

import FloydWarshall

import Test.HUnit
import Data.List (nub)
import Data.Map (Map,keys)
import Data.Ix (Ix)
import qualified Data.Map as Map

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
fromInt' g = toEnum

basicExchangeData :: Exchange
basicExchangeData = Exchange (Map.fromList d)
    where
      d = [((A,B), 0.88)
          ,((A,C), 1.10)
          ,((B,A), 1.20)
          ,((B,C), 0.15)
          ,((C,A), 0.89)
          ,((C,B), 5.10)]

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

test1 = TestCase (assertEqual "Basic test case 1" (Just [A,C,A]) (findArbitrage basicExchangeData))