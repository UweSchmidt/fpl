module Main
  -- (main)
where

import qualified FPPL
import           FP1.Value

type Value = FPPL.Value BV1
type Heap  = FPPL.Heap  BV1

main :: IO ()
main = do
  putStrLn "fp1: nothing yet implemented"
  return ()
