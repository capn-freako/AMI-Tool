module AMIParse where

import Text.ParserCombinators.Parsec

type AmiTree  = (String, [AmiToken])

type AmiToken = (String, AmiExp)

data AmiExp   = Val String
              | Tokens [AmiToken]
    deriving (Show)

identifier :: Parser String
identifier = many1 (alphaNum <|> char '_')

--real = many1 (digit <|> char '+' <|> char '-' <|> char '.')
real :: Parser Double
real = do
    n <- many1 (digit <|> char '+' <|> char '-' <|> char '.')
    return (read n)
{-do
    s <- char '-' <|> return '+'
    i <- many digit
    p <- char '.' <|> return (read (s:i))
    f <- many digit
    return (read ((s : i) ++ (p : f)))
-}

eol :: Parser Char
eol = char '\n'

skipJunk :: Parser ()
skipJunk = do
    skipMany space
    skipMany (do char '|'
                 manyTill anyChar eol
                 skipMany space)
    return ()

amiTree :: Parser AmiTree
amiTree = do
    skipJunk
    char '('
    skipJunk
    rootName <- identifier     <?> "root name"
    skipJunk
    tokens   <- many1 amiToken <?> "token list"
    skipJunk
    char ')'
    skipJunk
    return (rootName, tokens)

amiToken :: Parser AmiToken
amiToken = do char '('
              skipJunk
              lbl <- identifier <?> "label"
              skipJunk
              do tokens <- many1 amiToken
                 skipJunk
                 char ')'
                 skipJunk
                 return (lbl, Tokens tokens)
               <|> do val <- (quotedVal <|> many (satisfy (/= ')')))
                      char ')'
                      skipJunk
                      return (lbl, Val val)

quotedVal :: Parser String
quotedVal = do
    char '"'
    res <- many (satisfy (/= '"'))
    char '"'
    return res

