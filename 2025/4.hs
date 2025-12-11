----------------------
import Control.Monad
import System.IO
import Debug.Trace
import qualified Data.Text as T

import Fun

----------------------

day :: Int
day = 4

main = solve "4.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_lines path_to_file
    print $ solve_helper l

solve_helper :: [T.Text] -> Int
solve_helper l
  | length idxs > 0     = length idxs + solve_helper (foldl make_new_l l idxs)
  | otherwise           = 0

  where
    idxs =
      [
        (x, y)
        | y <- [0 .. length l - 1], x <- [0 .. T.length (l !! y) - 1],
        check_neighbors l x y < 4 && T.index (l !! y) x == '@'
      ]

    make_new_l :: [T.Text] -> (Int, Int) -> [T.Text]
    make_new_l grid (x, y) =
      let
        (prev, row : post) = splitAt y grid
        new_row =
          T.take x row
          <> T.singleton '.'
          <> T.drop (x + 1) row

      in prev ++ [new_row] ++ post

check_neighbors :: [T.Text] -> Int -> Int -> Int
check_neighbors grid x y = length $ filter check coords
  where
    coords = [(-1, 1), (-1, 0), (-1, -1), (0, 1), (0, -1), (1, 1), (1, 0), (1, -1)]

    check (dx, dy) =
      let (new_x, new_y) = (x + dx, y + dy)
      in is_valid new_x new_y && T.index (grid !! new_y) new_x == '@'

    is_valid i j =
      j >= 0
      && j < length grid
      && i >= 0
      && i < T.length (grid !! j)
