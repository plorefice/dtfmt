module Main where

import           AST
import           Format
import           Data.Either
import           Data.List
import           Text.Megaparsec

main :: IO ()
main = do
  dts <- getContents
  mapM_ putStrLn
    . format
    . fromRight (Source [] [])
    . parse parseSource ""
    $ dts
