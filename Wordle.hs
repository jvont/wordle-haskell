module Wordle (play, guess, solveFor, solveForM) where

import Control.Monad
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Random

-- Filter a list by comparing it to another list
filterOn :: (a -> b -> Bool) -> [a] -> [b] -> [a]
filterOn p xs = map fst . filter (uncurry p) . zip xs

-- Check whether a list has more than a max number of duplicates
maxDups :: (Eq a) => Int -> [a] -> Bool
maxDups k l = length l <= length (nub l) + k

{-
Wordle rules:
  You have 6 attempts to guess a 5-letter word
  Each guess must use the hints from previous attempts (hard mode)

Wordle returns the following hints for a guess and a solution:
  Correct - letters share the same position
  Present - letter is not in the correct position
  Absent  - letter does not exist

Starting word heuristic:
  Word containing the highest average letter frequencies, such that no
  duplicate letters are used ("later" is the go-to)
-}

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
hints :: String -> String -> [Hint]
hints s g = pres corr rem where
  -- correct hints
  corr = zipWith (\x y -> if x == y then Correct x else Absent x) g s
  -- remaining solution letters
  rem = filterOn (/=) s g
  -- scan words which are present but incorrectly guessed
  pres [] ys = []
  pres (Absent x:xs) ys
    | x `elem` ys = Present x:pres xs (delete x ys)
    | otherwise   = Absent  x:pres xs ys
  pres (x:xs) ys = x:pres xs ys

-- Check if a guess word matches hints
matches :: [Hint] -> String -> Bool
matches g w = corr g w && and (rest g rem) where
  -- matches correct/present hints
  corr (x:xs) (y:ys) = case x of
    Correct z -> y == z && corr xs ys
    Present z -> y /= z && corr xs ys
    _ -> corr xs ys
  corr _ _ = True
  -- remaining letters
  rem = filterOn p w g where
    p _ (Correct _) = False
    p _ _ = True
  -- matches present/absent hints
  rest [] ys = []
  rest (x:xs) ys = case x of
    Present y -> e y : rest xs (f y)
    Absent y -> not (e y) : rest xs ys
    _ -> rest xs ys
    where e = (`elem` ys)
          f x = if e x then delete x ys else ys

-- Filter possible solution words based on hint
solutions :: [Hint] -> [String] -> [String]
solutions = filter . matches

-- Scoring heuristic, the sum of letter frequencies for a given word
wordScore :: M.Map Char Int -> String -> Int
wordScore m = sum . map (\x -> M.findWithDefault 0 x m)

-- Rank words based on score
rankedWords :: M.Map Char Int -> [String] -> [String]
rankedWords m = sortBy (flip compare `on` wordScore m)

-- Letter frequences from a list of words
letterFreqs :: [String] -> M.Map Char Int
letterFreqs l = M.fromListWith (+) [(x, 1) | x <- concat l]

-- Get the best guess from a list of words
bestGuess :: [String] -> String
bestGuess d = let r = rankedWords (letterFreqs d) d in
  case find (maxDups 0) r of
    Just x -> x
    Nothing -> head r

-- Return the next best word based on hints, along with remaining solutions
solveHint :: [String] -> [Hint] -> (String, [String])
solveHint d h = (bestGuess d', d') where
  d' = solutions h d

-- Return the next best word based on a solution word's hints, along with remaining solutions
solveWord :: [String] -> String -> (String, [String])
solveWord d s = solveHint d $ hints s $ bestGuess d

-- Find a solution for a given word, return the number of guesses
solveFor :: [String] -> String -> Int
solveFor d s = go d s 0 where
  go [x] s n = n
  go d s n = go (snd (solveWord d s)) s (n + 1)

-- Find a solution, showing steps
solveForM :: [String] -> String -> IO Int
solveForM d s = go d s 0 where
  go d s n = do
    let (g, d') = solveWord d s
    if length d' == 1 then do
      print $ hints s (head d')
      return (n + 1)
    else do
      print $ hints s g
      go d' s (n + 1)

-- play loop
--   d = solution words
--   r = remaining solution words
--   s = solution word
--   n = attempts remaining
playLoop d r s n = do
  putStr $ show n ++ ": "
  g <- getLine
  if g `elem` d then do
    let h = hints s g
        r = solutions h d
    -- putStrLn $ "== " ++ show h ++ " (" ++ show (length r) ++ " remaining)"
    putStrLn $ "== " ++ show h
    if g == s
      then putStrLn "Great work!"
    else if n == 1
      then putStrLn $ "Uh oh! The right word was " ++ show s
    else playLoop d r s (n - 1)
  else do
    putStrLn "Invalid word!"
    playLoop d d s n

play = do
  content <- readFile "words.txt"
  -- get random word, solve
  gen <- getStdGen
  let d = lines content
      s = d !! fst (randomR (0, length d) gen)
  -- putStrLn $ "Turns to beat: " ++ show (solveFor d s)
  playLoop d d s 6

-- Process user input hint string
hintsFrom :: String -> String -> Maybe [Hint]
hintsFrom g h = zipWithM f h g where
  f c x = case c of
    'C' -> Just (Correct x)
    'P' -> Just (Present x)
    'A' -> Just (Absent  x)
    _   -> Nothing

getHints :: String -> [String] -> IO ()
getHints g d = do
  putStr "Input Wordle hints: "
  l <- getLine
  let h = hintsFrom g l
      go h = case h of
        Just i -> do
          let (g', d') = solveHint d i
          if all isCorrect i || length d == 1 then
            putStrLn $ "Answer is" ++ g'
          else do
            putStrLn $ "Next guess: " ++ g'
            getHints g' d'
        Nothing -> do
          putStrLn "Invalid hints, try again."
          getHints g d
        where
          isCorrect (Correct _) = True
          isCorrect _ = False
  go h

guessLoop d = do
  putStr "Guessed word: "
  g <- getLine
  getHints g d

guess = do
  content <- readFile "words.txt"
  let d = lines content
  putStrLn "Welcome to Wordle guesser!"
  putStrLn "Input Wordle hints in the following format:"
  putStrLn $ c ++ " - correct"
  putStrLn $ p ++ " - present"
  putStrLn $ a ++ " - absent"
  putStrLn $ "example: " ++ intercalate "" [c,p,c,a,a]
  putStrLn ""
  guessLoop d
  where
    c = show (Correct 'C')
    p = show (Present 'P')
    a = show (Absent  'A')

-- main = play
main = guess

