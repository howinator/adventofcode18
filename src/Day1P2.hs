module Day1P2
 ( day1p2
 ) where

import System.IO
import Data.List (group,sort)

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

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . group

-- | 'repeated' finds only the elements that are present more than once in the list. Example: 
--
--  repeated  "foo bar" == "o" 

repeated :: Ord a => [a] -> [a]
repeated = map head . filterByLength (>1)

day1p2 = do
  handle <- openFile "/Users/howie/my-projects/aoc18/input/day1puzzle1.txt" ReadMode -- lol hard code
  contents <- hGetContents handle
  let result = head $ repeated $  parse contents
  let resultStr = "Result is: " ++ (show result)
  putStrLn resultStr
  hClose handle
