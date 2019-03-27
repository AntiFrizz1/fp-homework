module Block3.Task4
  ( listlistParser
  ) where

import Block3.Task1 (Parser (..), runP)
import Block3.Task3 (charToInt, charToIntParser, parseFirst, isDigit)

import Control.Applicative (Alternative (..))

isWhite :: Char -> Bool
isWhite x
  | x == ' ' || x == '\n' || x == '\t' || x == '\r' = True
  | otherwise                                       = False

skipWhitespace :: a -> Parser Char a
skipWhitespace x = Parser $ \s ->
  case s of
    []   -> Just (x, [])
    a:as -> if (isWhite a)
      then runP (skipWhitespace x) as
      else Just (x, a:as)

skipComma :: a -> Parser Char a
skipComma x = Parser $ \s ->
  case s of
    []   -> Nothing
    a:as -> if (a == ',')
      then Just (x, as)
      else Nothing

otherIntParser :: (Int -> Int -> Int) -> Int -> Parser Char Int
otherIntParser p x = Parser $ \s ->
  case s of
    []     -> Just (x, [])
    (a:as) -> if (isDigit a)
      then runP (otherIntParser p (p (x * 10) (charToInt a))) as
      else Just (x, a:as)

addToListParser :: [Int] -> Parser Char [Int]
addToListParser x = Parser $ \s -> do
  (a, as) <- runP (parseFirst >>= (charToIntParser otherIntParser)) s
  Just (x ++ (a:[]), as)

listParser :: [Int] -> Int -> Parser Char [Int]
listParser l x
  | x == 0    = Parser $ \s -> Just (l, s)
  | x > 0     = Parser $ \s -> do
    (ql, qs) <- runP (skipComma l >>= skipWhitespace) s
    (nl, as) <- runP (addToListParser ql) qs
    (nnl, aas) <- runP (skipWhitespace nl) as
    runP (listParser nnl (x - 1)) aas
  | otherwise = empty

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

parseListNumber :: Parser Char [Int]
parseListNumber = parseFirst >>= charToIntParser (otherIntParser) >>=
  skipWhitespace >>= listParser []

listListParserF :: [[Int]] -> Parser Char [[Int]]
listListParserF l = Parser $ \s -> do
      (lq, qs) <- runP (skipWhitespace l) s
      if (isEmpty lq) && (isEmpty qs)
      then Just (lq, qs)
      else do
        (a, as)  <- runP parseListNumber qs
        (ls, bs) <- runP (skipWhitespace lq) as
        if (isEmpty bs)
        then Just (ls ++ (a:[]), [])
        else do
          (lw, ws) <- runP (skipComma ls) bs
          runP (listListParserF (lw ++ (a:[]))) ws

listlistParser :: Parser Char [[Int]]
listlistParser = listListParserF []
