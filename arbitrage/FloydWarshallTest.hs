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
