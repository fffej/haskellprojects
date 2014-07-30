module Biometrics.Face.EigenFaces where

import Biometrics.Face.FaceDatabase (faceDatabase)
import Numeric.Statistics.PCA
import Codec.BMP

import Data.Vector hiding (mapM,map)

import Control.Monad (liftM)
import Data.Either (rights)

readDatabase :: IO [BMP]
readDatabase = liftM rights $ mapM readBMP faceDatabase

convert :: [BMP] -> [Vector Double]
convert = map toVector

toVector :: BMP -> Vector Double
toVector = undefined
