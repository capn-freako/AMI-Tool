module AMIParse where

import Text.ParserCombinators.Parsec

type AmiToken = (String, AmiExp)

data AmiExp   = Vals [String]
              | Tokens [AmiToken]
    deriving (Show)

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

quotedVal :: Parser String
quotedVal = do
    char '"'
    res <- many (satisfy (/= '"'))
    char '"'
    return res

