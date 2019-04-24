{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( startNewInterpritator
  ) where

import Text.Megaparsec (Parsec, parse, atEnd, eof, many, getInput, try, some
  , (<|>), sepBy, sepEndBy, errorBundlePretty, satisfy, manyTill, count)
import Text.Megaparsec.Char (space, char, digitChar, alphaNumChar, string
  , space1, eol, letterChar)
import Data.Void (Void)
import Control.Monad.State.Lazy (State, state, execState)
import Control.Exception hiding(try)
import System.Directory( getHomeDirectory, getCurrentDirectory
  , setCurrentDirectory)
import System.Process (shell, readCreateProcessWithExitCode)
import System.Exit (ExitCode(..), exitWith)

type Parser = Parsec Void String

data Expression
  = OneString String
  | TwoString String
  | Variable String
  | Word String
  | Space
  | InlineCommands [[Expression]]
  | Dollar
  | BackSlash
  deriving (Show)

data Line
  = AssignedLine String [Expression]
  | CommentLine
  | ReadLine [String]
  | ErrorLine String
  | EndOfLine
  | EchoLine [Expression]
  | PresentWorkingDirectory
  | ChangeDirectory [Expression]
  | Command [Expression]
  | Exit [Expression]
  deriving (Show)

dollar :: Parser Char
dollar = do
  _ <- count 1 (char '\\')
  _ <- (char '$')
  return $ '$'

backSlash :: Parser Char
backSlash = do
  _ <- count 2 (char '\\')
  return $ '\\'

twoStringChar :: Parser Char
twoStringChar = do
  _ <- count 1 (char '\\')
  _ <- (char '"')
  return $ '"'

-- stringChar :: Parser Char
-- stringChar = do
--   _ <- count 1 (char '\\')
--   _ <- count 1 (char '"')
--   return $ '"'

notThat :: Char -> Parser Char
notThat c = satisfy (\s -> s /= c)

charLiteral :: Parser Char
charLiteral = satisfy (\s -> s /= '\'')

stringLiteral :: Parser String
stringLiteral = (try (dollar >>= (\_ -> return "\\$")))
 <|> try (backSlash >>= (\_ -> return "\\\\")) <|> try (twoStringChar >>=
   (\_ -> return "\"")) <|> (satisfy (\s -> s /= '\"') >>=
     (\s -> return $ s:[]))

twoStringLiteral :: Parser Expression
twoStringLiteral = do
  x <- (char '\"' >> manyTill stringLiteral (char '\"'))
  return $ TwoString $ concat x

oneStringLiteral :: Parser Expression
oneStringLiteral = do
  x <- (char '\'' >> manyTill charLiteral (char '\''))
  return $ OneString x

variable :: Parser String
variable = do
  x <- (letterChar <|> char '_')
  y <- many (alphaNumChar <|> char '_')
  return $ x:y

argVariable :: Parser String
argVariable = some digitChar

dollarVariable :: Parser Expression
dollarVariable = do
  _ <- char '$'
  x <- try (variable) <|> argVariable
  return $ Variable x

wordLiteral :: Parser Char
wordLiteral = try alphaNumChar
  <|> try (backSlash)
  <|> try (dollar)
  <|> try (twoStringChar)
  <|> satisfy (\s ->
    s /= '(' &&
    s /= ')' &&
    s /= '\'' &&
    s /= '\"' &&
    s /= '$' &&
    s /= '\\' &&
    s /= ' ' &&
    s /= '\n' &&
    s /= '\r' &&
    s /= '\t')


word :: Parser Expression
word = do
  x <- some wordLiteral
  return $ Word x

inlineCommand :: Parser [[Expression]]
inlineCommand = (some (oneStringLiteral <|> twoStringLiteral
 <|> try (dollarVariable) <|> word <|> spaceParse <|> inlineCommands))
 `sepBy` char ';'

inlineCommands :: Parser Expression
inlineCommands = do
  _ <- char '$'
  x <- char '(' >> inlineCommand <* char ')'
  return $ InlineCommands x

line :: Parser Line
line = do
  _ <- space
  x <- variable
  _ <- char '='
  y <- some (oneStringLiteral <|> twoStringLiteral <|> try (dollarVariable)
   <|> word <|> inlineCommands)
  _ <- many whiteSpaceWithoutNewLine
  _ <- ((eol >>= (\_ -> return ())) <|> eof <|> (char ';' >>=
    (\_ -> return ())))
  return $ AssignedLine x y

-- isEmptyList :: [a] -> Bool
-- isEmptyList [] = True
-- isEmptyList _  = False

readLine :: Parser Line
readLine = do
  _ <- space
  _ <- string "read"
  _ <- many whiteSpaceWithoutNewLine
  l <- parseStringList
  _ <- many whiteSpaceWithoutNewLine
  _ <- ((eol >>= (\_ -> return ())) <|> eof <|> (char ';' >>=
    (\_ -> return ())))
  return $ ReadLine l

parseStringList :: Parser [String]
parseStringList = (some (alphaNumChar)) `sepEndBy` (many (char ' '))

comment :: Parser String
comment = many (notThat '\n') <* ((eol >>= (\_ -> return ())) <|> eof)

commentLine :: Parser Line
commentLine = do
  _ <- space
  _ <- char '#'
  _ <- comment
  _ <- space
  return CommentLine

spaceParse :: Parser Expression
spaceParse = do
  _ <- some (char ' ')
  return $ Space

-- deleteOneSpace :: [Expression] -> [Expression]
-- deleteOneSpace [] = []
-- deleteOneSpace (Space:as) = as
-- deleteOneSpace a = a

echoLine :: Parser Line
echoLine = do
  _ <- space
  _ <- string "echo"
  x <- many whiteSpaceWithoutNewLine
  case x of
    [] -> do
      _ <- ((char '\n' >>= (\_ -> return ())) <|> eof <|> (char ';' >>=
        (\_ -> return ())))
      return $ EchoLine []
    _ -> do
      m <- many ((oneStringLiteral <|> twoStringLiteral <|> try (dollarVariable)
       <|> word <|> inlineCommands <|> spaceParse))
      _ <- many whiteSpaceWithoutNewLine
      _ <- ((char '\n' >>= (\_ -> return ())) <|> eof <|> (char ';' >>=
        (\_ -> return ())))
      return $ EchoLine m

pwd :: Parser Line
pwd = do
  _ <- space
  _ <- string "pwd"
  _ <- many whiteSpaceWithoutNewLine
  _ <- ((eol >>= (\_ -> return ())) <|> eof <|> (char ';' >>=
    (\_ -> return ())))
  return $ PresentWorkingDirectory

command :: Parser Line
command = do
  m <- some ((oneStringLiteral <|> twoStringLiteral <|> try(dollarVariable)
   <|> word <|> inlineCommands <|> spaceParse))
  _ <- many whiteSpaceWithoutNewLine
  _ <- ((eol >>= (\_ -> return ())) <|> eof <|> (char ';' >>= (\_ -> return ())))
  return $ Command m


notWhiteSpace :: Parser Char
notWhiteSpace = satisfy (\s -> s /= ' ' && s /= '\t' && s /= '\n' && s /= '\r')

whiteSpaceWithoutNewLine :: Parser Char
whiteSpaceWithoutNewLine = satisfy (\s -> s == ' ' || s == '\t' || s == '\r')

cd :: Parser Line
cd = do
  _ <- space
  _ <- string "cd"
  p <- many whiteSpaceWithoutNewLine
  case p of
    [] -> return $ ChangeDirectory []
    _ -> do
      x <- many ((oneStringLiteral <|> twoStringLiteral <|> try(dollarVariable)
       <|> word <|> inlineCommands <|> spaceParse))
      _ <- many whiteSpaceWithoutNewLine
      _ <- ((eol >>= (\_ -> return ())) <|> eof <|> (char ';' >>= (\_ -> return ())))
      return $ ChangeDirectory x

exit :: Parser Line
exit = do
  _ <- space
  _ <- string "exit"
  z <- many whiteSpaceWithoutNewLine
  p <- case z of
    [] -> return $ []
    _  -> many (oneStringLiteral <|> twoStringLiteral <|> try (dollarVariable)
     <|> word <|> inlineCommands)
  _ <- ((eol >>= (\_ -> return ())) <|> eof <|> (char ';' >>= (\_ -> return ())))
  return $ Exit p

-- update :: String -> String -> [(String, String)] -> [(String, String)]
-- update _ _ [] = []
-- update var value ((a, b):as) =
--   let q = if (var == a) then value else b
--   in (a, q):(update var value as)

set :: String -> String -> [(String, String)] -> [(String, String)]
set var value [] = [(var, value)]
set var value ((a, b):as) =
  if (var == a) then (a, value):as else (a, b):(set var value as)

getValue :: String -> [(String, String)] -> String
getValue _ [] = ""
getValue s ((a, b):as) = if (a == s) then b else getValue s as


dollarUpdate :: Parser Expression
dollarUpdate = do
  _ <- count 1 (char '\\')
  _ <- (char '$')
  return $ Dollar

backSlashUpdate :: Parser Expression
backSlashUpdate = do
  _ <- count 2 (char '\\')
  return $ BackSlash

stringUpdate :: [(String, String)] -> Parser (IO String)
stringUpdate vars = do
  x <- many (satisfy (\s -> s /= '$' && s /= '\\'))
  zs <- atEnd
  if (zs)
  then do return $ return $ x
  else do
    y <- (try (dollarUpdate) <|> try (backSlashUpdate) <|> try (dollarVariable)
     <|> inlineCommands)
    z <- case y of
      Dollar -> return $ return "$"
      Variable p -> return $ return $ getValue p vars
      InlineCommands p -> return $ inlineExecute p vars
      BackSlash -> return $ return "\\"
      _ -> return $ putStrLn "error" >>= (\_ -> exitWith (ExitFailure 1))
    q <- stringUpdate vars
    return $ z >>= (\zr -> q >>= (\qr -> return $ x ++ zr ++ qr))

replaceWhitespaces :: Parser String
replaceWhitespaces =
  do
    x <- many notWhiteSpace
    z <- atEnd
    if (z)
    then do return $ x
    else do
      _ <- space1
      q <- replaceWhitespaces
      return $ x ++ " " ++ q


-- linesParser :: Parser [Line]
-- linesParser = space >> (many (try (line) <|> try (commentLine) <|> readLine))
--   <* eof

newLine :: Parser Line
newLine = (try (line) <|> try (commentLine) <|> try (readLine)
  <|> try (echoLine) <|> try (pwd) <|> try (cd) <|> try (exit) <|> command)

test :: Parser (Line, String)
test = do
  x <- newLine
  _ <- space
  getInput >>= (\s -> return (x, s))

-- deleteChars :: Int -> String -> String
-- deleteChars 0 a = a
-- deleteChars _ [] = []
-- deleteChars n (_:as) = deleteChars (n - 1) as

myConcat :: [String] -> String
myConcat [] = ""
myConcat (a:[]) = a
myConcat (a:as) = a ++ " " ++ myConcat as

distributeData
  :: [String] -> [String] -> [(String, String)] -> [(String, String)]
distributeData (a:[]) b c = set a (myConcat b) c
distributeData (a:as) (b:bs) c = set a b (distributeData as bs c)
distributeData [] _ c = c
distributeData _ [] c = c

mainParser :: String -> Parser (Line, String) -> (Line, String)
mainParser input parser = do
  case input of
    [] -> (EndOfLine, "")
    _  -> case parse parser "" input of
      Left as -> (ErrorLine (errorBundlePretty as), "")
      Right as -> as

makeListOfLine :: String -> [Line] -> [Line]
makeListOfLine "" ls = ls
makeListOfLine input ls = let (l, s) = mainParser input test in
  (makeListOfLine s (l:ls))

reverseList :: [a] -> [a] -> [a]
reverseList [] help = help
reverseList (l:list) help = reverseList list (l:help)

concatExpressions :: [Expression] -> [(String, String)] -> IO String
concatExpressions (e) a = sequenceA (map (execExpresion a) e) >>=
  (\s -> return $ concat s)

execExpresion :: [(String, String)] -> Expression -> IO String
execExpresion a e = case e of
  TwoString b ->  case parse (stringUpdate a) "" b of
    Left er -> putStrLn (errorBundlePretty er) >>=
      (\_ -> exitWith (ExitFailure 1))
    Right q -> q
  OneString b -> return b
  Variable b -> return $ getValue b a
  Word b -> return b
  Space -> return " "
  InlineCommands s -> (inlineExecute s a) >>= (\sr ->
   case parse replaceWhitespaces "" sr of
     Left er -> putStrLn (errorBundlePretty er) >>=
      (\_ -> exitWith (ExitFailure 1))
     Right as -> return $ as)
  BackSlash -> return "\\"
  Dollar -> return "$"



inlineExecute :: [[Expression]] -> [(String, String)] -> IO String
inlineExecute es a =  sequenceA (map (\s -> (concatExpressions s a) >>= (\sq ->
 (readCreateProcessWithExitCode (shell sq) "") `catch` (\e ->
 return (ExitFailure 1, "", displayException (e :: IOError)))) >>=
   (\(ex, sr, er) -> case ex of
     ExitSuccess -> return $ sr
     ExitFailure _ -> putStr er >>= (\_ -> return sr))) es) >>=
       (\s -> return $ concat s)

parseArg :: Parser String
parseArg = do
  z <- string "-n"
  _ <- (((some whiteSpaceWithoutNewLine) >>= (\_ -> return ())) <|> eof)
  return z;

parseFirstEchoArg :: Parser String
parseFirstEchoArg = do
  z <- many (try (parseArg))
  case z of
    [] -> getInput >>= (\s -> return $  s ++ "\n")
    _ -> getInput >>= (\s -> return $ s)

number :: Parser Int
number = do
  _ <- space
  z <- many digitChar
  _ <- space
  _ <- eof
  case z of
    [] -> return 0
    a -> return $ (read a :: Int)


splitPath :: Parser [String]
splitPath = do
  z <- many (notThat '/')
  f <- atEnd
  if (f)
  then case z of
    [] -> return []
    a -> return (a:[])
  else do
    _ <- char '/'
    p <- splitPath
    return (z:p)

replaceHomeDirectory :: [String] -> IO (String)
replaceHomeDirectory [] = return ""
replaceHomeDirectory (a:[]) = getHomeDirectory >>= (\s -> case a of
  "~" -> return s
  p -> return p)
replaceHomeDirectory (a:as) = getHomeDirectory >>= (\s -> case a of
  "~" -> return s
  p -> return p) >>= (\s ->
   replaceHomeDirectory as >>= (\p -> return $ s ++ "/" ++ p))


execWithState
  :: State (IO [(String, String)]) [Line]
  -> State (IO [(String, String)]) [Line]
execWithState st = st >>= (\e -> case e of
  [] -> return []
  (l:ls) -> case l of
    CommentLine -> execWithState (state $ \y -> (ls, y))
    AssignedLine a b -> execWithState $ state $ \y -> (ls,
     y >>= (\ys -> concatExpressions b ys >>= (\s ->
     case parse replaceWhitespaces "" s of
       Left er -> putStrLn (errorBundlePretty er) >>= (\_ ->
        exitWith (ExitFailure 1))
       Right bs -> return $ set a bs ys)))
    ReadLine a -> execWithState $ state $ \y -> (ls, y >>= (\ys ->
     getLine >>= (\s ->
      case (parse (space >>= (\_ -> parseStringList)) "" s) of
        Left er -> putStrLn (errorBundlePretty er) >>= (\_ ->
         exitWith (ExitFailure 1))
        Right q -> return $ distributeData a q ys)))
    ErrorLine a -> execWithState $ state $ \y -> (ls, y >>= (\_ ->
     putStrLn (a) >>= (\_ -> exitWith (ExitFailure 1))))
    EchoLine a -> execWithState $ state $ \y -> (ls, y >>= (\ys ->
     (concatExpressions a ys >>= (\s -> case parse parseFirstEchoArg "" s of
       Left er -> putStrLn (errorBundlePretty er) >>= (\_ ->
        exitWith (ExitFailure 1))
       Right as -> putStr as)) >>= (\_ -> return ys)))
    EndOfLine ->  return []
    PresentWorkingDirectory -> execWithState $ state $ \y -> (ls, y >>= (\ys ->
     getCurrentDirectory >>= (\s -> putStrLn s >>= (\_ -> return $ys))))
    ChangeDirectory s -> execWithState $ state $ \y -> (ls, y >>= (\ys ->
     (concatExpressions s ys) >>= (\sw -> case sw of
       "" -> return "~"
       a -> return a) >>= (\ss -> (case parse splitPath "" ss of
      Left er -> putStrLn (errorBundlePretty er) >>= (\_ ->
       exitWith (ExitFailure 1))
      Right p -> replaceHomeDirectory p >>= (\sr -> setCurrentDirectory sr))
       `catch` (\er -> putStrLn (displayException (er :: IOError) ))) >>= (\_ ->
       return $ ys)))
    Exit a -> state $ \y -> ([], y >>= (\ys -> concatExpressions a ys >>= (\s ->
     case parse (number) "" s of
       Left _ -> putStrLn ("exit: " ++ s ++ " is not a number") >>= (\_ ->
        exitWith (ExitFailure 4))
       Right p -> case p of
                    0 -> exitWith (ExitSuccess)
                    q -> exitWith (ExitFailure q))))
    Command a -> execWithState $ state $ \y -> (ls, y >>= (\ys ->
     concatExpressions a ys >>= (\s ->
     (readCreateProcessWithExitCode (shell s) "") `catch` (\er ->
     return (ExitFailure 5, "", displayException (er :: IOError)))) >>=
       (\(ex, s, er) -> case ex of
         ExitSuccess -> return $ s
         ExitFailure _ -> putStr er >>= (\_ -> return s)
         )  >>= (\s -> putStr s) >>= (\_ -> return ys))))

startNewInterpritator :: String -> [(String, String)] -> IO [(String, String)]
startNewInterpritator input st = execState (execWithState (
 return (reverseList (makeListOfLine input []) []))) (return st)
