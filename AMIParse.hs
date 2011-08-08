module AMIParse where

import Text.ParserCombinators.Parsec
import AMITypes

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

symbol :: Parser a -> Parser a
symbol p = do
    skipMany space
    skipMany (do char '|'
                 manyTill anyChar eol
                 skipMany space)
    res <- p
    skipMany space
    skipMany (do char '|'
                 manyTill anyChar eol
                 skipMany space)
    return res

amiTree :: Parser AmiTree
amiTree = do
    symbol (char '(')             <?> "openning parenthesis"
    rootName <- symbol identifier <?> "root name"
    tokens   <- many1 amiToken    <?> "token list"
    symbol (char ')')             <?> "closing parenthesis"
    return (rootName, tokens)

amiToken :: Parser AmiToken
amiToken = do
    symbol (char '(')             <?> "openning parenthesis"
    lbl <- symbol identifier      <?> "label"
    do tokens <- try (many1 amiToken)
       symbol (char ')')          <?> "closing parenthesis"
       return (lbl, TokenList tokens)
       <|> do val <- symbol (amiVal lbl)
              symbol (char ')')   <?> "closing parenthesis"
              return (lbl, Attribute val)
       <?> ("token list, or value (Label: " ++ lbl ++ ")")

amiVal :: String -> Parser AmiAttribute
amiVal attType = case attType of
    "Description" -> do
        char '"'
        desc <- many (satisfy (/= '"'))
        char '"'
        return (Description desc)

    "Usage" -> do
        use <- identifier
        case use of
            "Info"  -> return (Usage Info)
            "In"    -> return (Usage In)
            "Out"   -> return (Usage Out)
            "Inout" -> return (Usage Inout)
            _       -> return (Usage (UsrUsage use))

    "Type" -> do
        pType <- identifier
        case pType of
            "Bool" -> return (Type PBool)
            "Int"  -> return (Type PInt)
            "Tap"  -> return (Type PTap)
            "UI"   -> return (Type PUI)
            _      -> return (Type PUsr)

    "Default" -> do
        val <- real
        return (Default (AmiDefVal val))
        
    "Format" -> do
        frmtType <- identifier
        case frmtType of
            "Range" -> do
                val1 <- real
                val2 <- real
                val3 <- real
                return (Format (Range val1 val2 val3))

            _       -> do
                return (Format (UsrFrmt frmtType))

    _ -> do
        str <- identifier <?> "user defined attribute label"
        return (UsrAtt attType str)

