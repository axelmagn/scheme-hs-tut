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
             deriving(Show)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


parseExpr :: Parser LispVal
parseExpr = parseCharacter
         <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_-"


spaces :: Parser ()
spaces = skipMany1 space


parseAtom :: Parser LispVal
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
    _ <- char '"'
    xs <- many strChar
    _ <- char '"'
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
            digits <- string "#o" >> many1 octDigit
            return $ fst . head . readOct $ digits
        hexadecimal = do
            digits <- string "#x" >> many1 hexDigit
            return $ fst . head . readHex $ digits
        decimal = do
            digits <- optional (string "#d") >> many1 digit
            return $ fst . head . readDec $ digits


parseCharacter :: Parser LispVal
parseCharacter = do
    _ <- string "#\\"
    c <- try parseCharacterName <|> anyChar
    return $ Character c
    where
        parseCharacterName :: Parser Char
        parseCharacterName = do
            name <- many1 alphaNum
            case name of
                "space" -> return ' '
                "newline" -> return '\n'
                _ -> fail "Unrecognized Character Name"


parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
    headExprs <- endBy parseExpr spaces
    tailExpr <- char '.' >> spaces >> parseExpr
    return $ DottedList headExprs tailExpr


parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' >> parseExpr
    return $ List [Atom "quote", x]
