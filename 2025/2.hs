----------------------
import Control.Monad
import System.IO
import qualified Data.Text as T

import Fun

----------------------

day :: Int
day = 2

main = solve "2.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_commas path_to_file
    print $ solve_helper l

solve_helper :: [T.Text] -> Int
solve_helper [] = 0
solve_helper (x : xs) =
  res + solve_helper xs

  where
    [start, stop] = T.splitOn (T.pack "-") x
    res = check_range (read $ T.unpack start) (read $ T.unpack stop)

check_range :: Int -> Int -> Int
check_range start stop
  | start > stop      = 0
  | otherwise         = check_mod 2 + check_range (start + 1) stop

    where
      s = T.pack $ show start
      len = T.length s

      check_mod mod_by
        | mod_by > len            = 0
        | len `mod` mod_by == 0   =
          let n = len `div` mod_by

          in if T.replicate mod_by (T.take n s) == s
            then start
            else check_mod (mod_by + 1)

        | otherwise               = check_mod (mod_by + 1)
