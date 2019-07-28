module Main
  -- (main)
where

import qualified FPL
import           FP0.Value

type Value = FPL.Value BV0
type Heap  = FPL.Heap  BV0

main :: IO ()
main = do
  putStrLn "fpplc: Nothing yet implemented"
  return ()
