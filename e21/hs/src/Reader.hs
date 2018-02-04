module Reader where

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Data.Attoparsec.Text as P

import Model

try :: String -> IO ()
try file = do
  body <- T.readFile file
  T.putStrLn body
  case parseBody body of
    Right games' -> do
      print games'
    Left msg -> do
      T.putStrLn msg

readSample :: String -> IO (Either Text [Game])
readSample file = do
  body <- T.readFile file
  pure $ parseBody body

showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . T.pack $ show r

parseBody :: Text -> Either Text [Game]
parseBody x = showParseResult $ parse (bodyParser <* endOfInput) x `feed` ""



bodyParser :: Parser [Game]
bodyParser = many line
  where
    line = do
      skipWhile (/= '"')
      s <- char '"' *> scene <* char '"'
      skipWhile (/= '"')
      expected <- char '"' *> slots <* char '"'
      skipWhile (/= '\n') <* endOfLine
      pure (s, expected)

    scene = do
      ms <- many1 (living <|> dying)
      char '/'
      n <- decimal
      pure (ms, makeSlots n, 0)

    living = flip (,) Living . read . pure <$> digit
    dying = flip (,) Dying . read . pure <$> (char '[' *> digit <* char ']')

    slots = do
      ss <- many ((char '0' >> pure Empty) <|> (char '1' >> pure Loaded))
      pure $ V.fromList ss
