module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Char (string)
import Numeric (readOct, readDec, readHex)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Character Char
             | Bool Bool


main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value"


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_-"


spaces :: Parser ()
spaces = skipMany1 space


parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom


parseString :: Parser LispVal
parseString = do
    char '"'
    -- x <- many (noneOf "\"") <|> many (string "\\\"")
    xs <- many strChar
    char '"'
    let str = concat xs
    return $ String str
    where strChar :: Parser String
          strChar = escape <|> fmap return nonEscape
          nonEscape :: Parser Char
          nonEscape = noneOf "\""
          escape :: Parser String
          escape = do
            d <- char '\\'
            c <- anyChar
            return [d, c]


parseNumber :: Parser LispVal
parseNumber = do
    num <- octal <|> hexadecimal <|> decimal
    return $ Number num
    where
        octal = do
            string "#o"
            digits <- many1 $ oneOf "01234567"
            return $ fst . head . readOct $ digits
        decimal = do
            optional $ string "#d"
            digits <- many1 $ oneOf "0123456789"
            return $ fst . head . readDec $ digits
        hexadecimal = do
            string "#x"
            digits <- many1 $ oneOf "0123456789abcdefABCDEF"
            return $ fst . head . readHex $ digits


parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
