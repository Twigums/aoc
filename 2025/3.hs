----------------------
import Control.Monad
import System.IO
import Debug.Trace
import qualified Data.Text as T

import Fun

----------------------

day :: Int
day = 3

main = solve "3.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_lines path_to_file
    print $ solve_helper l

solve_helper :: [T.Text] -> Int
solve_helper [] = 0
solve_helper (x : xs) =
  -- trace (T.unpack x ++ " " ++ show res) (res + solve_helper xs)
  res + solve_helper xs

  where
    res = get_max x

get_max :: T.Text -> Int
get_max s
  | T.length s < max_len = 0
  | otherwise = read $ T.unpack $ build s max_len
    where
      max_len = 12

      build :: T.Text -> Int -> T.Text
      build rest len
        | len == 0 || T.null rest   = T.empty
        | T.length rest == len      = rest
        | otherwise                 =
          let
            max_char = T.maximum $ T.take (T.length rest - len + 1) rest
            (x, xs) = T.breakOn (T.singleton max_char) rest
            new_rest = T.drop 1 xs

          in T.singleton max_char <> build new_rest (len - 1)
