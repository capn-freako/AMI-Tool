module AMIParse (
    AmiToken
  , AmiExp (Vals, Tokens)
  , showTree
  , amiGetInt
  , getAmiExp
  , amiToken
  , module Text.ParserCombinators.Parsec
) where

import Text.ParserCombinators.Parsec
import Data.List

-- New data type and its helper functions
type AmiToken = (String, AmiExp)

data AmiExp   = Vals [String]
              | Tokens [AmiToken]
instance Show AmiExp where
    show = showTree ""

showTree :: String -> AmiExp -> String
showTree _ (Vals strs)        = " " ++ (intercalate " " strs) ++ "\n"
showTree indent (Tokens toks) = "\n" ++ (concat $ map (showToken (' ' : indent)) toks)

showToken :: String -> AmiToken -> String
showToken indent tok = indent ++ "(" ++ (fst tok) ++ (showTree indent (snd tok)) ++ indent ++ ")\n"

amiGetInt :: AmiExp -> Maybe Int
amiGetInt (Tokens _)    = Nothing
amiGetInt (Vals [])     = Nothing
amiGetInt (Vals (x:xs)) = Just (read x)

-- This is the AMI specific parser.
amiToken :: Parser AmiToken
amiToken = do skipJunk
              symbol (char '(')
              lbl <- symbol (identifier <?> "label")
              do tokens <- try (symbol (many1 amiToken))
                 symbol (char ')')
                 return (lbl, Tokens tokens)
               <|> do vals <- sepBy (quotedVal <|> many (noneOf " )")) (char ' ')
                      symbol (char ')')
                      return (lbl, Vals vals)

-- These are helper functions.
identifier :: Parser String
identifier = many1 (alphaNum <|> char '_')

real :: Parser Double
real = do
    n <- many1 (digit <|> char '+' <|> char '-' <|> char '.')
    return (read n)

eol :: Parser Char
eol = char '\n'

whitespace :: Parser String
whitespace = many1 space

skipJunk :: Parser ()
skipJunk = do
    skipMany space
    skipMany (do char '|'
                 manyTill anyChar eol
                 skipMany space)
    return ()

symbol :: Parser a -> Parser a
symbol p = do res <- p
              skipJunk
              return res

quotedVal :: Parser String
quotedVal = do
    char '"'
    res <- many (satisfy (/= '"'))
    char '"'
    return $ '"' : res ++ "\""

-- getter function for callers to use
getAmiExp :: AmiToken -> [String] -> Maybe AmiExp
getAmiExp _ [] = Nothing
getAmiExp amiToken (l:ls)
    | fst amiToken == l =
          case ls of
              [] -> Just (snd amiToken)
              _  -> case snd amiToken of
                        Tokens tokens -> case [t | t <- tokens, fst t == head ls] of
                                             [token] -> getAmiExp token ls
                                             _       -> Nothing
                        _             -> Nothing
    | otherwise = Nothing

