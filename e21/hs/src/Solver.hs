module Solver where

import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Vector (Vector, (//), (!?))
import qualified Data.Vector as V

import Model


formatMove :: Move -> String
formatMove (x, Living) = "(" ++ show x ++ ")"
formatMove (x, Dying) = "[" ++ show x ++ "]"

formatSlot :: Slot -> String
formatSlot Unknown = "_"
formatSlot Empty = "0"
formatSlot Loaded = "1"

formatSlots :: Vector Slot -> String
formatSlots slots = concat $ formatSlot <$> slots

formatScene :: Scene -> String
formatScene (ms, slots, i) = intercalate " "
  [ formatSlots slots
  , show i
  , formatMoves
  ]
  where
    formatMoves = concat $ formatMove <$> ms

solve :: Scene -> [History]
solve scene = go [scene]

go :: History -> [History]
go [] = []
go h@(scene : _) = concat $ go' <$> step scene
  where
    go' next = case (isValid next, elem next h, isFinished next) of
      (True, False, True) -> [next : h]
      (True, False, False) -> go (next : h)
      _ -> []

isValid :: Scene -> Bool
isValid (ms, slots, _) = dyingCount <= unknownCount
  where
    dyingCount = length $ filter ((== Dying) . snd) ms
    unknownCount = length $ V.filter (== Unknown) slots

isFinished :: Scene -> Bool
isFinished (ms, _, _) = all ((== Living) . snd) ms

step :: Scene -> [Scene]
step (m@(i, Living) : ms, slots, cur)
  | slots !? cur == Just Unknown =
    [(ms <> [m], slots // [(cur, Empty)], (cur + i) `mod` length slots)]
  | otherwise =
    [(ms <> [m], slots, (cur + i) `mod` length slots)]
step (m@(i, Dying) : ms, slots, cur)
  | slots !? cur == Just Unknown =
    [ (ms, slots // [(cur, Loaded)], cur)
    , (ms <> [m], slots // [(cur, Empty)], (cur + i) `mod` length slots)
    ]
  | otherwise =
    [(ms <> [m], slots, (cur + i) `mod` length slots)]


fillEmpty :: Scene -> Scene
fillEmpty (ms, slots, i) = (ms, V.map update' slots, i)
  where
    update' Unknown = Empty
    update' x = x
