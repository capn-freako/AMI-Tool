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

amiToken :: Parser AmiToken
amiToken = do skipJunk
              char '('
              skipJunk
              lbl <- identifier <?> "label"
              skipJunk
              do tokens <- try (many1 amiToken)
                 skipJunk
                 char ')'
                 skipJunk
                 return (lbl, Tokens tokens)
               <|> do vals <- sepBy (quotedVal <|> many (noneOf " )")) (char ' ')
                      char ')'
                      skipJunk
                      return (lbl, Vals vals)

quotedVal :: Parser String
quotedVal = do
    char '"'
    res <- many (satisfy (/= '"'))
    char '"'
    return res

