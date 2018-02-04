module E21
  ( run
  , makeSlots
  , module Model
  , module Solver
  , module Reader
  ) where

import Control.Monad

import Model
import Solver
import Reader


run :: Scene -> IO ()
run scene = do
  putStrLn ""
  displayHistory . head $ solve scene
  -- putStrLn "OK"

displayHistory :: History -> IO ()
displayHistory h@(last : _) = do
  putStrLn "-----"
  void . traverse displayScene $ reverse h
  displayScene $ fillEmpty last
  where
    displayScene = putStrLn . formatScene
