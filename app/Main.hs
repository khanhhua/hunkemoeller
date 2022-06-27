module Main where

import Control.Monad
import Network.Simple.TCP
import Data.List (intersperse)
import System.Directory
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import Data.Maybe


data FileMeta =
  FileMeta
    { name :: String
    , permissions :: Permissions
    , size :: Integer
    }

instance Show FileMeta where
  show (FileMeta name permissions size) = name <> ", " <> showPermissions permissions <> ", " <> show size

showPermissions :: Permissions -> String
showPermissions p = permissionsToFlag readable p 
                  <> permissionsToFlag writable p 
                  <> permissionsToFlag executable p
                  <> permissionsToFlag searchable p
  where
    permissionsToFlag f = show . fromEnum . f

showRows :: Show a => [a] -> String
showRows xs = unwords . intersperse "\n" $ map show xs

getFileMeta :: FilePath -> IO FileMeta
getFileMeta p = FileMeta p <$> getPermissions p <*> getFileSize p

showDir :: FilePath -> IO [FileMeta]
showDir dir = listDirectory dir >>= mapM getFileMeta

catFile :: FilePath -> IO String
catFile fname = undefined

main :: IO ()
main = withSocketsDo $ do
  serve (Host "127.0.0.1") "8000" accept
  where
    accept (socket, remoteAddr) = do
      putStrLn $ "TCP connection established from " <> show remoteAddr
      send socket $ UTF8.fromString "Welcome to Hunkemoeller"
      forever $ do
        received <- fromMaybe "" <$> recv socket 1024
        putStrLn $ Char8.unpack received
        send socket $ UTF8.fromString "OK\n"
