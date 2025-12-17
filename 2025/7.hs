----------------------
import Control.Monad
import System.IO
import Debug.Trace
import Data.List (findIndices, findIndex)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M

import Fun

----------------------

day :: Int
day = 7

main = solve "7.txt"

solve :: FilePath -> IO ()
solve path_to_file =
  do
    l <- Fun.split_lines path_to_file
    print $ solve_helper l M.empty

solve_helper :: [T.Text] -> M.Map Int Int -> Int
solve_helper [] store = sum $ M.elems store
solve_helper (line : lines) store =
  let
    line_str = T.unpack line
    splitter_locations = S.fromAscList $ findIndices (== '^') line_str

    new_store =
      if store == M.empty
        then
          case findIndex (== 'S') line_str of
            Just a  -> M.singleton a 1
            Nothing -> M.empty

        else
          M.fromListWith (+)
          $ concatMap (split_on_idx splitter_locations)
          $ M.toList store

  in
    solve_helper lines $ new_store

split_on_idx :: S.Set Int -> (Int, Int) -> [(Int, Int)]
split_on_idx splitters (idx, x) =
  if S.member idx splitters
    then [(idx - 1, x), (idx + 1, x)]
    else [(idx, x)]
