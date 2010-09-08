module Example where

import Em6502
import Data.IORef

main :: IO ()
main = do
  cpu <- mkCPU -- Create a blank CPU
  writeIORef (ac cpu) 4 -- store 4 in the accumulator
  let instructions = [ASL Accumulator
                     ,STA (Absolute 1024)
                     ,ASL Accumulator
                     ,ASL Accumulator
                     ,CLC
                     ,ADC (Absolute 1024)]
  runInstructions cpu instructions
  ac' <- readIORef (ac cpu)
  word <- readWord cpu 1024
  print ac'
  return ()