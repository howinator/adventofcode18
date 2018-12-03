module Day1P1
 ( day1p1
 ) where

import System.IO

parse :: String -> [Int]
parse inputStr = 
  let tupleList :: [String] -> [(Char, String)]
      tupleList listOfStrings = [(head line, tail line) | line <- listOfStrings]
      freqChangeNumberList :: [(Char, String)] -> [Int]
      sign '+' = 1
      sign '-' = -1
      sign _ = 0
      freqChangeNumberList listOfTuples = [ (sign (fst ele)) * (read (scnd ele) :: Int) | ele <- listOfTuples, 
        let scnd (_,a) = a ]
  in  freqChangeNumberList $ tupleList $ lines inputStr

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

day1p1 = do
  handle <- openFile "/Users/howie/my-projects/aoc18/input/day1puzzle1.txt" ReadMode -- lol hard code
  contents <- hGetContents handle
  let result = sumList $  parse contents
  let resultStr = "Result is: " ++ (show result)
  putStrLn resultStr
  hClose handle
