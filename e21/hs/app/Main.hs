module Main where

import Data.Foldable (traverse_)
import E21

scene :: Scene
scene = ([(3, Living), (2, Dying), (3, Living)], makeSlots 6, 0)

main :: IO ()
main = do
  traverse_ displayHistory $ E21.solve scene
