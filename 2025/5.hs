----------------------
import Control.Monad
import System.IO
import Debug.Trace
import Data.Ord (comparing)
import Data.List (sortBy, find)
import qualified Data.Text as T

import Fun

----------------------

day :: Int
day = 5

main = solve "5.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_lines path_to_file
    print $ solve_helper l

solve_helper :: [T.Text] -> Int
solve_helper l =
  -- in_ranges parsed_second combined_ranges
  second_part combined_ranges

  where
    (first, second) = span (/= T.empty) l
    parsed_second = map (read . T.unpack) (filter (/= T.empty) second) :: [Int]

    sorted_ranges = sort_ranges first
    combined_ranges = combine_ranges sorted_ranges

sort_ranges :: [T.Text] -> [(Int, Int)]
sort_ranges ranges = sortBy (comparing fst) $ map get_pair ranges

  where
    get_pair x =
      let [a, b] = T.splitOn (T.singleton '-') x
      in (read (T.unpack a), read (T.unpack b))

combine_ranges :: [(Int, Int)] -> [(Int, Int)]
combine_ranges [] = []
combine_ranges [x] = [x]
combine_ranges ((prev_a, prev_b) : (a, b) : rest)
  | a <= prev_b + 1 = combine_ranges ((prev_a, max prev_b b) : rest)
  | otherwise       = (prev_a, prev_b) : combine_ranges ((a, b) : rest)

in_ranges :: [Int] -> [(Int, Int)] -> Int
in_ranges [] _ = 0
in_ranges (n : ns) ranges =
  case find (\(a, b) -> n >= a && n <= b) ranges of
    Just _ -> 1 + in_ranges ns ranges
    Nothing -> in_ranges ns ranges

second_part [] = 0
second_part ((a, b) : ranges) = b - a + 1 + second_part ranges
