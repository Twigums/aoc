module Fun where

import qualified Data.Text as T

split_lines :: FilePath -> IO [T.Text]
split_lines path_to_file =
  do
    raw <- readFile path_to_file
    return $ T.splitOn (T.pack "\n") (T.pack raw)

split_commas :: FilePath -> IO [T.Text]
split_commas path_to_file =
  do
    raw <- readFile path_to_file
    return $ T.splitOn (T.pack ",") (T.pack raw)
