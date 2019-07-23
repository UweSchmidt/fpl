module Main
  -- (main)
where

import qualified FPPL
import           FP0.Value

type Value = FPPL.Value BV0
type Heap  = FPPL.Heap  BV0

main :: IO ()
main = do
  putStrLn "fpplc: Nothing yet implemented"
  return ()
