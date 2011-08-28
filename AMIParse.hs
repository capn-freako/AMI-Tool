module AMIParse where

import ApplicativeParsec
import Data.List

type AmiToken = (String, AmiExp)

data AmiExp   = Vals [String]
              | Tokens [AmiToken]
instance Show AmiExp where
    show = showTree ""

showTree :: String -> AmiExp -> String
showTree _ (Vals strs)        = " " ++ intercalate " " strs ++ "\n"
showTree indent (Tokens toks) = '\n' : concatMap (showToken ('\t' : indent)) toks

showToken :: String -> AmiToken -> String
showToken indent tok = indent ++ "(" ++ fst tok ++ showTree indent (snd tok) ++ indent ++ ")\n"

-- This is the AMI specific parser.
amiToken :: Parser AmiToken
amiToken = liftA2 (,) (skipJunk *> symbol (char '(') *> symbol (identifier <?> "label"))
                      (try (Tokens <$> symbol (many1 amiToken))
                       <|> Vals <$> (quotedVal <|> many (noneOf " )")) `sepBy` char ' ' <* symbol (char ')'))

-- These are helper functions.
identifier :: Parser String
identifier = many1 (alphaNum <|> char '_')

real :: Parser Double
real = read <$> many1 (digit <|> char '+' <|> char '-' <|> char '.')

eol :: Parser Char
eol = char '\n'

whitespace :: Parser String
whitespace = many1 space

skipJunk :: Parser ()
skipJunk = skipMany space *> skipMany (char '|' *> anyChar `manyTill` eol *> skipMany space)

symbol :: Parser a -> Parser a
symbol p = p <* skipJunk

quotedVal :: Parser String
quotedVal = char '"' *> liftA quoteTok (many (satisfy (/= '"'))) <* char '"'
quoteTok :: String -> String
quoteTok s = '"' : s ++ "\""

