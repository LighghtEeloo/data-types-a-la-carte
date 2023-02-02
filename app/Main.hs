module Main where
import Syntax

main :: IO ()
main = do
  print $ evalExpr $ add (val 1) (val 2)
