module Lib where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

data Property = Property String Value deriving (Show, Eq)

data Value
    = Empty
    | U32 Integer
    | Str String
    | Ref String
    | Array [Value]
    | List [Value]
    deriving (Show, Eq)

data Stmt
    = N Node
    | P Property
    deriving (Show, Eq)

data Node = Node String [Stmt] deriving (Show, Eq)

{- Grammar utility functions -}

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme (L.decimal <|> L.hexadecimal)

comma :: Parser String
comma = symbol ","

{- Grammar parsing functions -}

validNodeChar :: Parser Char
validNodeChar = alphaNumChar <|> oneOf ",._+-"

validPropChar :: Parser Char
validPropChar = alphaNumChar <|> validNodeChar <|> oneOf "?#"

validLabelChar :: Parser Char
validLabelChar = alphaNumChar <|> char '_'

{- Node name and unit address -}

nodeident :: Parser String
nodeident = (lexeme . try) (root <|> internal)
  where
    root     = string "/"
    internal = (:) <$> letterChar <*> MP.many validNodeChar

unitaddr :: Parser String
unitaddr = lexeme $ MP.many hexDigitChar

nodename :: Parser String
nodename = (lexeme . try) (refval <|> name)
  where
    name    = (++) <$> nodeident <*> optaddr
    optaddr = option "" $ (:) <$> char '@' <*> unitaddr

{- Properties -}

propname :: Parser String
propname = (lexeme . try) ((:) <$> letterChar <*> MP.many validPropChar)

u32 :: Parser Value
u32 = U32 <$> integer

str :: Parser Value
str = do
    symbol "\""
    s <- MP.many $ satisfy (`notElem` "\"")
    symbol "\""
    return $ Str s

refval :: Parser String
refval = (:) <$> char '&' <*> MP.many validLabelChar

ref :: Parser Value
ref = Ref <$> refval

array :: Parser Value
array = do
    symbol "<"
    p <- MP.many $ u32 <|> str <|> ref
    symbol ">"
    return $ Array p

list :: Parser Value
list = f <$> sepBy1 allowed comma
  where
    f l = if length l == 1 then head l else List l
    allowed = u32 <|> str <|> ref <|> array

value :: Parser Value
value = u32 <|> str <|> array <|> list

prop :: Parser Stmt
prop = do
    n <- propname
    symbol "="
    v <- value
    symbol ";"
    return . P $ Property n v

{- Node -}

node :: Parser Stmt
node = do
    n <- nodename
    symbol "{"
    s <- MP.many $ (try node) <|> (try prop)
    symbol "}"
    symbol ";"
    return . N $ Node n s

