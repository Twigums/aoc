----------------------
import Control.Monad
import System.IO
import qualified Data.Text as T

import Fun

----------------------

day :: Int
day = 1

main = solve "1.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_lines path_to_file
    print $ solve_helper l 50 0

solve_helper :: [T.Text] -> Int -> Int -> Int
solve_helper [] _ counter = counter
solve_helper (x : xs) current counter =
  solve_helper xs new_current new_counter

  where
    (direction, amount) =
      case (T.uncons x) of
        Just (x, y)   -> (x, y)
        Nothing       -> ('R', T.pack "0")

    (new_current, new_counter) = spin_dial current direction (read $ T.unpack amount) counter

spin_dial :: Int -> Char -> Int -> Int -> (Int, Int)
spin_dial current direction amount counter = (remainder, new_counter)

  where
    check_direction =
      if direction == 'R'
        then (+)
        else (-)

    new_val = check_direction current amount

    negative_cross =
      if new_val < 0 && current > 0
        then 1
        else 0

    crossed = abs new_val `div` 100
    remainder = new_val `mod` 100

    is_crossed_zero =
      if crossed > 0 && new_val == 0
        then -1
        else 0

    is_zero =
      if new_val == 0
        then 1
        else 0

    new_counter = negative_cross + crossed + is_crossed_zero + is_zero + counter
