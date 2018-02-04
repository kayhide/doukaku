module Main where

import qualified Data.Text.IO as T
import Data.Foldable (traverse_)

import E21

scene :: Scene
scene = ([(3, Living), (2, Dying), (3, Living)], makeSlots 6, 0)

main :: IO ()
main = do
  games <- readSample "data/sample.txt"
  case games of
    Right games' -> traverse_ test games'
    Left msg -> T.putStrLn msg

test :: Game -> IO ()
test (scene, expected) = do
  run scene
  putStrLn $ formatSlots expected
