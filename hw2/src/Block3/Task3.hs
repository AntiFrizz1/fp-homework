module Block3.Task3
  ( parseBrackets
  , parseNumber
  , isDigit
  , parseFirst
  , charToInt
  , charToIntParser
  ) where

import Block3.Task1 (Parser (..), runP)
import Block3.Task2 (satisfy, ok)
import Data.Char (ord)
import Control.Applicative (Alternative (..))

runBracketParser :: Int -> Parser Char Int
runBracketParser x = Parser $ \s ->
  case s of
    []      -> Just (x, [])
    (a:as)  ->
      case a of
        '(' -> runP (runBracketParser (x + 1)) as
        ')' -> if (x == 0)
          then Nothing
          else runP (runBracketParser (x - 1)) as
        _   -> Nothing


validateBracketsParser :: Parser Char Int -> Parser Char ()
validateBracketsParser p = p >>= (\x -> if (x == 0) then ok else empty)

parseBrackets :: Parser Char ()
parseBrackets = validateBracketsParser (runBracketParser 0)

isDigit :: Char -> Bool
isDigit c
  | (c >= '0' && c <= '9') = True
  | otherwise              = False

parseFirst :: Parser Char Char
parseFirst = satisfy f
  where
    f c
      | (isDigit c) || c == '+' || c == '-' = True
      | otherwise                           = False


charToInt :: Char -> Int
charToInt c = (ord c) - (ord '0')

charToIntParser
  :: ((Int -> Int -> Int) -> Int -> Parser Char Int)
  -> Char
  -> Parser Char Int
charToIntParser parser x
  | x == '+'  = Parser $ \s ->
    case s of
      [] -> Nothing
      _  -> runP (parser (+) 0) s
  | x == '-'  = Parser $ \s ->
    case s of
      [] -> Nothing
      _  -> runP (parser (-) 0) s
  | isDigit x = Parser $ \s ->
    case s of
      [] -> Just (charToInt x, [])
      _  -> runP (parser (+) (charToInt x)) s
  | otherwise = empty

intParser :: (Int -> Int -> Int) -> Int -> Parser Char Int
intParser p x = Parser $ \s ->
  case s of
    []     -> Just (x, [])
    (a:as) -> if (isDigit a)
      then runP (intParser p (p (x * 10) (charToInt a))) as
      else Nothing

parseNumber :: Parser Char Int
parseNumber = parseFirst >>= (charToIntParser intParser)
