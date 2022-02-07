module Wordle (play, guess, solve) where

import Control.Monad
import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.Random

-- Filter a list by comparing it to another list
filterOn :: (a -> b -> Bool) -> [a] -> [b] -> [a]
filterOn p xs = map fst . filter (uncurry p) . zip xs

-- Check whether a list has more than a max number of duplicates
maxDups :: (Eq a) => Int -> [a] -> Bool
maxDups k l = length l <= length (nub l) + k

-- Wordle rules:
--   You have 6 attempts to guess a 5-letter word
--   Each guess must use the hints from previous attempts (hard mode)

-- Wordle returns the following hints for a guess and a solution:
--   Correct - letters share the same position
--   Present - letter is not in the correct position
--   Absent  - letter does not exist

-- Starting word heuristic:
--   Word containing the highest average letter frequencies, such that no
--   duplicate letters are used ("later" is the go-to)

data Hint = Correct Char | Present Char | Absent Char deriving(Eq)

-- Print hints, color coded
instance Show Hint where
  show c = case c of
    Correct x -> bgGreen  ++ x:tcClear
    Present x -> bgYellow ++ x:tcClear
    Absent  x -> tcClear  ++ x:tcClear
    where tcClear  = "\ESC[0m"
          tcBlack  = "\ESC[30m"
          bgGreen  = "\ESC[42m" ++ tcBlack
          bgYellow = "\ESC[43m" ++ tcBlack
  showList cs = (concatMap show cs ++)

-- Hints for a given solution and guess word
getHints :: String -> String -> [Hint]
getHints soln guess = allHints where
  -- correct hints
  corrHints = zipWith isCorr guess soln where
    isCorr x y
      | x == y = Correct x
      | otherwise = Absent x
  -- find incorrect guess letters which are present
  allHints = isPres corrHints guessNotCorr where
    isPres [] ys = []
    isPres (Absent x:xs) ys
      | x `elem` ys = Present x:isPres xs (delete x ys)
      | otherwise   = Absent  x:isPres xs ys
    isPres (x:xs) ys = x:isPres xs ys
    guessNotCorr = filterOn (/=) soln guess

-- Check if a guess word matches hints
matches :: [Hint] -> String -> Bool
matches hints word = matchesCorr && matchesPresAbs where
  -- matches correct/present hints
  matchesCorr = corr hints word where
    corr (x:xs) (y:ys) = case x of
      Correct z -> y == z && corr xs ys
      Present z -> y /= z && corr xs ys
      _ -> corr xs ys
    corr _ _ = True
  -- matches present/absent hints
  matchesPresAbs = presAbs hints notCorr where
    notCorr = filterOn p word hints where
      p _ (Correct _) = False
      p _ _ = True
    presAbs [] ys = True
    presAbs (x:xs) ys = case x of
      Present y -> (y `elem` ys) && presAbs xs (delete y ys)
      Absent y -> (y `notElem` ys) && presAbs xs ys
      _ -> presAbs xs ys

-- Scoring heuristic, the sum of letter frequencies for a given word
wordScore :: M.Map Char Int -> String -> Int
wordScore scores = sum . map (\x -> M.findWithDefault 0 x scores)

-- Rank words based on score
rankedWords :: M.Map Char Int -> [String] -> [String]
rankedWords scores = sortBy (flip compare `on` wordScore scores)

-- Letter frequences from a word dictionary
letterFreqs :: [String] -> M.Map Char Int
letterFreqs dict = M.fromListWith (+) [(x, 1) | x <- concat dict]

-- Get the best guess from a word dictionary
bestGuess :: [String] -> String
bestGuess dict = let r = rankedWords (letterFreqs dict) dict in
  case find (maxDups 0) r of
    Just x -> x
    Nothing -> head r

-- Return the next best word based on hints, along with remaining solutions
solveHint :: [String] -> [Hint] -> (String, [String])
solveHint dict hints =
  let dict' = filter (hints `matches`) dict
  in  (bestGuess dict', dict')

-- Return the next best word based on a solution word's hints, along with remaining solutions
solveWord :: [String] -> String -> (String, [String])
solveWord dict soln = solveHint dict $ getHints soln $ bestGuess dict

-- Find a solution for a given word, return the number of guesses
solve :: [String] -> String -> Maybe Int
solve dict soln = go dict soln 0 where
  go [] _ _ = Nothing
  go d s n
    | length d' == 1 = Just (n + 1)
    | otherwise = go d' s (n + 1)
    where d' = snd (solveWord d s)

-- play loop
--   dict = solution words
--   rest = rest of possible words
--   soln = solution word
--   rem = remaining attempts
playLoop :: [String] -> [String] -> String -> Int -> IO ()
playLoop dict rest soln rem = do
  putStr $ show rem ++ ": "
  g <- map toLower <$> getLine
  if g `elem` dict || g `elem` rest then do
    putStrLn $ "== " ++ show (getHints soln g)
    if g == soln
      then putStrLn "Great work!"
    else if rem == 1
      then putStrLn $ "Uh oh! The right word was " ++ show soln
    else playLoop dict rest soln (rem - 1)
  else do
    -- return to last line, clear
    putStr "\ESC[1A\ESC[K\r"
    putStrLn "Invalid word!"
    playLoop dict rest soln rem

play = do
  dict <- lines <$> readFile "solutions.txt"
  rest <- lines <$> readFile "words.txt"
  -- get random word, solve
  gen <- getStdGen
  let soln = dict !! fst (randomR (0, length dict) gen)
  playLoop dict rest soln 6

-- Process user input hint string
hintsFrom :: String -> String -> Maybe [Hint]
hintsFrom guess hintStr = zipWithM toHint hintStr guess where
  toHint h g = case h of
    'C' -> Just (Correct g)
    'P' -> Just (Present g)
    'A' -> Just (Absent  g)
    _   -> Nothing

guessLoop :: [String] -> String -> IO ()
guessLoop dict guess = do
  putStr "Guessed word (press Enter to skip): "
  inputGuess <- map toLower <$> getLine
  let newGuess = if null inputGuess then guess else inputGuess
  putStr "Input Wordle hints: "
  hintString <- map toUpper <$> getLine
  putStrLn ""
  case hintsFrom newGuess hintString of
    Just hint -> do
      let (guess', dict') = solveHint dict hint
      case dict' of
        [] -> putStrLn "No solution found!"
        [x] -> putStrLn $ "Answer is: " ++ show x
        _ -> do
          putStrLn $ "Next guess: try " ++ guess' ++ " (1/" ++ show (length dict') ++ ")"
          guessLoop dict' guess'
    Nothing -> do
      putStrLn "Invalid hints, try again."
      guessLoop dict guess

guess = do
  solutions <- readFile "solutions.txt"
  let dict = lines solutions
  putStrLn $ unlines [
    "Welcome to Wordle guesser!",
    "Input Wordle hints in the following format:",
    c ++ " - correct",
    p ++ " - present",
    a ++ " - absent",
    "example: " ++ intercalate "" [c,p,c,a,a]
    ]
  let guess = bestGuess dict
  putStrLn $ "First guess: try " ++ show guess
  guessLoop dict guess
  where
    c = show (Correct 'C')
    p = show (Present 'P')
    a = show (Absent  'A')

main = do
  args <- getArgs
  case args of
    ["play"] -> play
    ["guess"] -> guess
    _ -> do
      case args of
        [] -> putStrLn "No argument provided."
        _ -> putStrLn "Invalid argument(s)."
      putStrLn "Usage: wordle (play | guess)"
