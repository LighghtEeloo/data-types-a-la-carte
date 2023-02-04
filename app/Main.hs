module Main where
import Inject(main0)
import FreeRun(main1)

main :: IO ()
main = do
  Inject.main0
  FreeRun.main1
