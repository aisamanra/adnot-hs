module Main where

import Data.Adnot
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Environment (getArgs)
import System.Exit (die)

helpText :: String
helpText = "Usage: adnot-id [file]"

main = do
  content <- do
    args <- getArgs
    case args of
      [] -> BS.getContents
      ["-"] -> BS.getContents
      [file] -> BS.readFile file
      _ -> die helpText
  case decodeValue content of
    Right val -> BSL.putStrLn (encodeValue val)
    Left err -> die err
