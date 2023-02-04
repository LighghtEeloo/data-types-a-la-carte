module Main where
import Inject(main0)
import FreeRun(main1)

main :: IO ()
main = do
  putStrLn "=== Inject ==="
  Inject.main0
  putStrLn "=== FreeRun ==="
  FreeRun.main1
