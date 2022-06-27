module Main where

import System.Directory

data FileMeta =
  FileMeta
    { name :: String
    , permissions :: Permissions
    , size :: Integer
    }
  deriving (Show)

getFileMeta :: FilePath -> IO FileMeta
getFileMeta p = FileMeta p <$> getPermissions p <*> getFileSize p

showDir :: FilePath -> IO [FileMeta]
showDir dir = do
  ps <- listDirectory dir
  mapM getFileMeta ps

catFile :: FilePath -> IO String
catFile fname = undefined

main :: IO ()
main = undefined
