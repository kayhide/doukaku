module Main where

import           ClassyPrelude        hiding (many, some, try)

import           Data.Aeson           (FromJSON)
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Void            (Void)
import           Text.Megaparsec      (Parsec, errorBundlePretty, many, oneOf,
                                       parse, parseMaybe, some, try)
import           Text.Megaparsec.Char (char, digitChar, lowerChar, upperChar)



main :: IO ()
main = do
  Event { test_data = problems } <- loadData dataFile
  -- print event
  -- traverse_ attempt problems
  let answers = intercalate "," . solve <$> problems
  traverse_ (uncurry check) $ zip answers problems

check :: Text -> Problem -> IO ()
check answer Problem { number, src, expected } = do
  bool wrong right $ answer == expected
  where
    wrong :: IO ()
    wrong =
      putStrLn $ tshow number <> " Wrong: " <> src <> " => " <> answer <> " /= " <> expected

    right :: IO ()
    right =
      putStrLn $ tshow number <> " Right: " <> src <> " => " <> answer

attempt :: Problem -> IO ()
attempt Problem { src } =
  case parse pathP "" src of
    Left bundle -> putStrLn . pack $ errorBundlePretty bundle
    Right _     -> pure ()

solve :: Problem -> [Text]
solve Problem { src } =
  fromMaybe ["-"] $ parseMaybe pathP src


-- * Parser combinators

type Parser = Parsec Void Text

-- | Parser for an entry

entryP :: Parser Text
entryP = do
  ent <- fmap mconcat $ some $ try (plainString <|> quatedString '\'' <|> quatedString '\"')
  when ("" == ent) $ fail "unexpected empty entry"
  pure ent

  where
    safeChar :: Parser Char
    safeChar = try (lowerChar <|> upperChar <|> digitChar <|> (char '/' *> char '/'))

    plainString :: Parser Text
    plainString = pack <$> some safeChar

    quatedString :: Char -> Parser Text
    quatedString q = fmap pack $
      char q *> many (try (oneOf ('/' : delete q quatableChars) <|> safeChar)) <* char q

    quatableChars :: [Char]
    quatableChars = ['\'', '\"']


-- | Parser for a path with more than 1 entry

pathP :: Parser [Text]
pathP = (:) <$> entryP <*> many (char '/' *> entryP)


-- * Event file handling

dataFile :: FilePath
dataFile = "data/data.json"

data Event =
  Event
  { event_id  :: Text
  , event_url :: Text
  , test_data :: [Problem]
  }
  deriving (Show, Eq, Generic)
instance FromJSON Event

data Problem =
  Problem
  { number   :: Int
  , src      :: Text
  , expected :: Text
  }
  deriving (Show, Eq, Generic)
instance FromJSON Problem


-- | Load problem items.
loadData :: FilePath -> IO Event
loadData file = do
  json <- readFile file
  pure $ fromMaybe (error "Failed to load items") $ Aeson.decode $ BSL.fromStrict json
