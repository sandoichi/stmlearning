module Main where

import Data.Maybe
import MatchNumbers (run)
import Text.Printf (printf)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

promptFor :: Read a => String -> IO a
promptFor msg = do
  putStrLn msg
  val <- getLine
  case maybeRead val of
    Nothing -> putStrLn "Invalid value." *> promptFor msg
    Just x -> pure x


main :: IO ()
main = do
  numCount <- promptFor "How many random numbers should we generate?"
  workerCount <- promptFor "How many random workers should we use?"
  putStrLn $ printf "Generating %i random numbers and starting %i workers"
    numCount workerCount

  run numCount workerCount
