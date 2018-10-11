module Main where

import           AST
import           Text.Megaparsec

main :: IO ()
main = do
  dts <- getContents
  parseTest parseSource dts
