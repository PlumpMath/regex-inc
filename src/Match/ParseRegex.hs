module ParseRegex where

import Text.ParserCombinators.Parsec 
import System.Environment

import Regex
import Data.Set


reserved :: String
reserved = ".()[]*|"

parseBracketLiteral :: Parser Char
parseBracketLiteral = ((char '\\' >> oneOf "[]") <|> noneOf "[]" <?> "literal") 

parseCharClass :: Parser String
parseCharClass = (try $ string "a-z" >> return ['a'..'z'])
                 <|> (try $ string "A-Z" >> return ['A'..'Z'])
                 <|> (try $ string "0-9" >> return ['0'..'9'])
                 <|> many1 parseBracketLiteral
                 <?> "bracket literals"
                    


parseLiteral :: Parser Regex
parseLiteral = ((char '\\' >> oneOf reserved) <|> noneOf reserved <?> "literal") >>= (return . Literal)

parseUnion :: Parser Regex
parseUnion = do char '(' 
                leftRegex <- parseToken 
                char '|'
                rightRegex <- parseToken
                char ')'
                return $ Union leftRegex rightRegex
             <?> "union"

parseRegexStar :: Parser Regex
parseRegexStar = do char '('
                    regex <- parseToken
                    string ")*"
                    return $ Star regex

parseLiteralStar :: Parser Regex
parseLiteralStar = do literalRegex <- parseLiteral
                      char '*'
                      return $ Star literalRegex

parseStar :: Parser Regex
parseStar = try parseRegexStar <|> parseLiteralStar <?> "star"

parseBracket :: Parser Regex 
parseBracket = do char '['
                  try $ lookAhead $ noneOf "^"
                  str <- many1 parseCharClass 
                  char ']'
                  return $ OneOf $ fromList (concat str)

parseToken :: Parser Regex
parseToken = chainl (try parseUnion <|> try parseStar <|> try parseBracket <|> parseLiteral) (return Seq) Epsilon 

parseRegex :: Parser Regex
parseRegex = do regex <- parseToken
                eof
                return regex
                        

stringToRegex :: String -> Regex
stringToRegex input = case parse parseRegex "string to regex" input of
                           Left err -> error $ show err
                           Right val -> val
