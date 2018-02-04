module Main where

import E21

scene :: Scene
scene = ([(3, Living), (2, Dying), (3, Living)], makeSlots 6, 0)

main :: IO ()
main = E21.run scene
