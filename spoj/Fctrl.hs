-- Success
import Control.Monad (forM_)

readInteger :: String -> Integer
readInteger = read

readInt :: String -> Int
readInt = read

-- http://www.purplemath.com/modules/factzero.htm
factSub :: Integer -> Integer
factSub n = sum $ takeWhile (>= 1) $ (drop 1 $ iterate (`div` 5) n)

main :: IO ()
main = do
  nStr <- readLn :: IO Integer
  forM_ [1..nStr] 
       (\t -> 
        do
          nS <- getLine
          let n = readInteger nS
              ans = factSub n
          print ans
       )
