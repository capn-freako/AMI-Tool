module Main where

import AMIParse
import Text.ParserCombinators.Parsec

main = do
    contents <- getContents
    case parse amiToken "(stdin)" contents of
        Left e -> do putStrLn "Error parsing input:"
                     print e
        Right r -> print r

