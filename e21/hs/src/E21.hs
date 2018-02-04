module E21
  ( module Model
  , module Solver
  , module Reader
  , displayHistory
  ) where

import Control.Monad

import Model
import Solver
import Reader


displayHistory :: History -> IO ()
displayHistory h@(last : _) = do
  void . traverse displayScene $ reverse h
  displayScene $ fillEmpty last
  putStrLn "-----"
  where
    displayScene = putStrLn . formatScene
