module Model where

import Data.Vector (Vector)
import qualified Data.Vector as V

data Livity = Living | Dying
  deriving (Eq, Show)

data Slot = Unknown | Empty | Loaded
  deriving (Eq, Show)

type Cur = Int
type Move = (Int, Livity)
type Scene = ([Move], Vector Slot, Cur)
type History = [Scene]

type Game = (Int, Scene, Vector Slot)


makeSlots :: Int -> Vector Slot
makeSlots i = V.replicate i Unknown
