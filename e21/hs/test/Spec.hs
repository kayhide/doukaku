module Main where

import Control.Lens
import qualified Data.Text.IO as T
import Data.Foldable (traverse_)

import E21


main :: IO ()
main = do
  -- games <- readSample "data/sample-non_deterministic.txt"
  games <- readSample "data/sample.txt"
  case games of
    Right games' -> traverse_ test games'
    Left msg -> T.putStrLn msg

test :: Game -> IO ()
test (no, scene, expected) = do
  print no
  traverse_ displayResult histories
  putStrLn "----------"

  where
    displayResult [] = pure ()
    displayResult history@(x : _) = do
      let result = view _2 $ fillEmpty x
      let okng = if result == expected then "OK" else  "NG"

      displayHistory history
      putStrLn $ formatSlots result
      putStrLn $ formatSlots expected ++ " " ++ okng

    histories = solve scene
