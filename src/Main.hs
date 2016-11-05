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
main = getArgs >>= print . eval . readExpr . head


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val


-- Parsing
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


-- Display
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Character ' ') = "#\\space"
showVal (Character '\n') = "#\\newline"
showVal (Character c) = "#\\" ++ show c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


-- Evaluation
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+))
             ,("-", numericBinop (-))
             ,("*", numericBinop (*))
             ,("/", numericBinop div)
             ,("mod", numericBinop mod)
             ,("quotient", numericBinop quot)
             ,("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then 0
                                else fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0
