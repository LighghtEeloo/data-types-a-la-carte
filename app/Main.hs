module Main where
import Inject(main)
import FreeRun(main)

main :: IO ()
main = do
  Inject.main
  FreeRun.main
