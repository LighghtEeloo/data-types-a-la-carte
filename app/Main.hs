module Main where
import Inject

main :: IO ()
main = do
  print $ evalExpr $ add (val 1) (val 2)
