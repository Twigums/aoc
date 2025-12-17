----------------------
import Control.Monad
import System.IO
import Debug.Trace
import Data.List (unsnoc, transpose)
import qualified Data.Text as T

import Fun

----------------------

day :: Int
day = 6

main = solve "6.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_lines path_to_file
    print $ solve_helper l

solve_helper :: [T.Text] -> Int
solve_helper l = operate transposed_lines

  where
    (lines, _) =
      case unsnoc l of
        Just (a, b) -> (a, b)
        Nothing     -> ([], T.empty)

    gaps = map T.length $ T.groupBy (\_ c -> c == ' ') (last lines)
    separated_lines = map (variable_split gaps) lines
    transposed_lines = transpose separated_lines

variable_split :: [Int] -> T.Text -> [T.Text]
variable_split [] texts =
  if T.null texts
    then []
    else [texts]

variable_split (n : ns) texts =
  chunk : variable_split ns rest

  where
    (chunk, rest) = T.splitAt n texts

operate :: [[T.Text]] -> Int
operate [] = 0
operate (line : rest) = res + operate rest

  where
    (vals, op) =
      case unsnoc line of
        Just (a, b) -> (a, b)
        Nothing     -> ([], T.empty)

    char_lists = map (map (:[]) . T.unpack) vals

    digit_rows = transpose char_lists

    combined_vals =
      map read
      . filter (not . null)
      . map (filter (/= ' ') . concat)
      $ digit_rows

    res =
      if T.strip op == T.singleton '+'
        then sum combined_vals
        else product combined_vals
