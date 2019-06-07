{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Task6
    ( FS(..)
    , getDirectory'
    , _nameLens
    , getSubtreeList
    , getDirName
    , getFileName
    , setRoot
    , getFirstDir
    , getFiles
    , _contents
    , _dirName
    , _fileName
    ) where

import Lens.Micro (Lens, (^.), (.~), (^?), Traversal)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist,
 canonicalizePath)
import Control.Exception (Exception(..), throwIO)
import Data.Typeable (Typeable)
import Data.List (reverse, takeWhile)
import Task5 (lens)

data IncorrectFilePath = IncorrectFilePath
    deriving (Show, Typeable, Exception)

data FS
    = Dir
    { name     :: FilePath  -- название папки, не полный путь
    , contents :: [FS]
    }
    | File
    { name     :: FilePath  -- название файла, не полный путь
    } deriving (Show)

getDirectory' :: FilePath -> IO FS
getDirectory' path = preparePath path >>= (\(p, n) -> getDirectoryHelp p n)
  where
    getDirectoryHelp :: FilePath -> FilePath -> IO FS
    getDirectoryHelp pa na = doesFileExist pa >>= (\s -> case s of
      True -> return $ File na
      False -> doesDirectoryExist pa >>= (\st -> case st of
        True -> listDirectory pa >>= (\sr ->
         sequenceA (fmap (\n -> getDirectoryHelp (pa ++ "/" ++ n) n) sr) >>= (\p ->
          return $ Dir na p))
        False -> throwIO IncorrectFilePath))

    preparePath :: FilePath -> IO (FilePath, FilePath)
    preparePath pa = canonicalizePath pa >>= (\s ->
     return (s, reverse (takeWhile  ((/=) '/') (reverse s))))


_nameLens :: Lens FS FS FilePath FilePath
_nameLens = lens name (\obj -> \newName -> obj { name = newName})

_changeDirLens :: Lens FS FS FilePath FilePath
_changeDirLens = lens name (\obj -> \newName -> case obj of
  File _ -> obj
  Dir _ _ -> obj { name = newName })

_contents :: Traversal FS FS [FS] [FS]
_contents func obj = case obj of
  File n -> pure (File n)
  Dir n c -> pure (Dir n) <*> func c

_dirName :: Traversal FS FS FilePath FilePath
_dirName func obj = case obj of
  Dir n c -> pure (\p -> Dir p c) <*> func n
  File p  -> pure (File p)

_fileName :: Traversal FS FS FilePath FilePath
_fileName func obj = case obj of
  Dir n c -> pure (Dir n c)
  File p  -> pure File <*> func p

getSubtreeList :: FS -> [FS]
getSubtreeList tree = tree ^. _contents

getDirName :: FS -> Maybe FilePath
getDirName tree = tree ^? _dirName

getFileName :: FS -> FilePath
getFileName tree = tree ^. _fileName

setRoot :: FS -> FS
setRoot tree = (.~) _changeDirLens "/" tree

getFirstDir :: FS -> Maybe FilePath
getFirstDir tree = getDirName tree >>= (\_ ->
 case span ((==) Nothing) (fmap (getDirName) (getSubtreeList tree)) of
   (_, a:_) -> a
   (_, _) -> Nothing)

getFiles :: FS -> [FilePath]
getFiles tree = foldr (\a -> \b -> case getFileName a of
  [] -> b
  x -> x:b) [] (getSubtreeList tree)
