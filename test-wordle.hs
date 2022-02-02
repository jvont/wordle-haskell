import Data.Function (on)
import Data.List
import Text.Printf
import Data.Maybe (mapMaybe)
import qualified Wordle as W

-- List average (from: https://stackoverflow.com/a/2377067/15837840)
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main = do
  content <- readFile "solutions.txt"
  let dict = lines content
      avgAll = average $ mapMaybe (W.solveFor dict) dict :: Double
  -- Test solveM
  putStrLn "Search for \"shire\" using solveM:"
  avg <- W.solveForM dict "shire"
  putStrLn $ "Took " ++ show avg ++ " turns"
  -- Solver average performance
  printf "Average turns taken across all solutions: %.2f\n" avgAll
  