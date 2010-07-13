{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FloydWarshallTest where

import FloydWarshall

import Test.HUnit
import Data.Map (Map)
import Data.Ix (Ix)
import qualified Data.Map as Map

data Units = A | B | C | D | E | F | G 
             deriving (Read,Show,Ord,Ix,Eq,Enum)
type Exchange = (Units,Units)
type ExchangeData = Map (Units,Units) Double

data ForexExchange = ForexExchange ExchangeData

instance Graph ForexExchange Units where
    vertices = vertices'
    edge = edge'
    fromInt = fromInt'

vertices' forexExchange = undefined

edge' = undefined

fromInt' = undefined
