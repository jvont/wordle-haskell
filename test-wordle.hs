import Data.Function (on)
import Data.List
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import qualified Wordle as W

import Control.Monad (mapM)

-- List average (from: https://stackoverflow.com/a/2377067/15837840)
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main = do
  content <- readFile "solutions.txt"
  let dict = lines content
      avg = average $ mapMaybe (W.solve dict) dict :: Double
  -- Solver average performance
  putStrLn "Average turns taken across all solutions:"
  printf "%.2f turns\n" avg
  