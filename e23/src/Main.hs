module Main where

import           ClassyPrelude

import           Control.Lens

data Edge = Forward | RightUp | LeftUp | Backward | LeftDown | RightDown
  deriving (Eq, Show, Bounded, Enum)

type Rule = Char

-- |
-- >>> Forward <> RightDown
-- RightDown
instance Semigroup Edge where
  e1 <> e2 = toEnum $ (fromEnum e1 + fromEnum e2) `mod` (fromEnum (maxBound :: Edge) + 1)


-- |
-- >>> toEdges 'a'
-- [Forward,RightUp,RightDown,Forward]
-- >>> toEdges 'b'
-- [Forward,RightUp,Forward,RightDown,Forward]
toEdges :: Rule -> [Edge]
toEdges r = case r of
  'a' -> [Forward, RightUp, RightDown, Forward]
  'b' -> [Forward, RightUp, Forward, RightDown, Forward]
  _   -> [Forward]


-- |
-- >>> go [RightUp] ['a']
-- [RightUp,LeftUp,Forward,RightUp]
-- >>> go [Forward] ['a', 'a']
-- [Forward,RightUp,RightDown,Forward,RightUp,LeftUp,Forward,RightUp,RightDown,Forward,LeftDown,RightDown,Forward,RightUp,RightDown,Forward]
go :: [Edge] -> [Rule] -> [Edge]
go es [] = es
go es (r : rs) = go (apply (toEdges r)) rs
  where
    apply patterns = (<>) <$> es <*> patterns

test  :: Int -> [Rule] -> Char -> Bool
test i rules x = do
  let es = go [Forward] rules
  case (es ^? ix (i - 1), x) of
    (Just Forward, '0')   -> True
    (Just Backward, '0')  -> True
    (Just RightUp, '+')   -> True
    (Just LeftDown, '+')  -> True
    (Just RightDown, '-') -> True
    (Just LeftUp, '-')    -> True
    (Nothing, 'x')        -> True
    _                     -> False


main :: IO ()
main = do
  print $ test 120 "aabb" '0'
  print $ test 100 "a" 'x'
  print $ test 3 "a" '-'
  print $ test 3 "b" '0'
  print $ test 9 "aa" '-'
  print $ test 10 "bb" '+'
  print $ test 11 "ab" '-'
  print $ test 12 "ba" '0'
  print $ test 7 "aaa" '0'
  print $ test 17 "baa" '+'
  print $ test 28 "bba" '-'
  print $ test 82 "bba" '+'
  print $ test 35 "baa" '-'
  print $ test 254 "babb" '+'
  print $ test 462 "abba" 'x'
  print $ test 226 "bbba" '0'
  print $ test 345 "bbba" '0'
  print $ test 256 "aaaa" '0'
  print $ test 11 "aaab" '-'
  print $ test 241 "abaab" '-'
  print $ test 490 "aabaa" '0'
  print $ test 1305 "bbbaa" '0'
  print $ test 1102 "ababa" '-'
  print $ test 1077 "abbab" '-'
  print $ test 281 "aabaa" '-'
  print $ test 2218 "abbaaa" '+'
  print $ test 4095 "bbabbb" '+'
  print $ test 2750 "abbaab" '+'
  print $ test 5573 "bbaaba" '+'
  print $ test 6644 "aaabba" 'x'
  print $ test 8109 "bbbbba" '+'
  print $ test 3860 "aaaabbb" '+'
  print $ test 59222 "bbbbbba" '0'
  print $ test 14956 "baabbab" '-'
  print $ test 14894 "ababbba" '+'
  print $ test 3163 "aaaaaab" '-'
  print $ test 21917 "babaaaa" '+'
  print $ test 178620 "aabbbaab" 'x'
  print $ test 96709 "babbaaaa" '0'
  print $ test 74116 "abababaa" '-'
  print $ test 22025 "abbbbabb" '0'
  print $ test 8612 "aaaabbaa" '-'
  print $ test 153868 "bbbabbab" '-'
  print $ test 747769 "abbabaaba" 'x'
  print $ test 541589 "baabbbbab" '-'
  print $ test 787443 "ababbbbab" '-'
  print $ test 129549 "ababaaaaa" '0'
  print $ test 837323 "aabbbabab" 'x'
  print $ test 140592 "bbbbabbab" '+'
  print $ test 219669 "ababbabbab" '-'
  print $ test 500261 "bbababaabb" '-'
  print $ test 966503 "aaabababbb" '0'
  print $ test 443603 "baabaababb" '+'
  print $ test 3912 "aabbababaa" '0'
  print $ test 2926358 "bbabbbbaba" '0'
  print $ test 18104279 "bbbaababbab" '-'
  print $ test 3849980 "aaabaaaaaba" '0'
  print $ test 9276072 "baabaabaaab" '0'
  print $ test 11202113 "baaaaabbbba" '0'
  print $ test 5432578 "abaabbaaaaa" '-'
  print $ test 17363025 "bbaabababbb" '0'
  print $ test 24147656 "baabaabbbbab" '0'
  print $ test 1078733 "bbbaaaabbbbb" '+'
  print $ test 38623426 "abaabababbaa" '-'
  print $ test 19312285 "bbaababbaaba" '+'
  print $ test 11485959 "baaaaababaaa" '-'
  print $ test 36831104 "babbbbbbabab" '+'
