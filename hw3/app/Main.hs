module Main where

import Lib (startNewInterpritator)
import System.Environment (getArgs)

getFilenameAndVariables :: [String] -> (String, [String])
getFilenameAndVariables a = (head a, tail a)

makePairs :: Int -> [String] -> [(String, String)] -> [(String, String)]
makePairs _ [] ans = ans
makePairs q (a:as) ans = makePairs (q + 1) as ((show q, a):ans)

main :: IO ()
main = do
  x <- getArgs
  let (file, array) = (getFilenameAndVariables x)
  readFile file >>= (\s -> startNewInterpritator s (makePairs 1 array []))
   >>= (\_ -> return ())
