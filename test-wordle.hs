import Data.Function (on)
import Data.List
import Text.Printf
import qualified Wordle

-- List average (from: https://stackoverflow.com/a/2377067/15837840)
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

main = do
  content <- readFile "words.txt"
  let d = lines content
      a = average $ map (Wordle.solve d) d :: Double
  -- Test solveM
  putStrLn "Search for \"shire\" using solveM:"
  n <- Wordle.solveM d "shire"
  putStrLn $ "Took " ++ show n ++ " turns"
  -- Solver average performance
  printf "Average turns taken across all words: %.2f" a
