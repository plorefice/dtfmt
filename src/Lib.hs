module Lib
    ( parseDTS
    )
where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

data Property = Property String Value deriving (Show, Eq)

data Value
    = Empty
    | U32 String
    | Str String
    | Ref String
    | Def String
    | List [Value]
    | Array [Value]
    deriving (Show, Eq)

type Label = String

data Stmt
    = N Node
    | P Property
    deriving (Show, Eq)

data Node = Node (Maybe Label) String [Stmt] deriving (Show, Eq)

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

hex :: Parser String
hex = (++) <$> string "0x" <*> some hexDigitChar

dec :: Parser String
dec = some digitChar

number :: Parser String
number = lexeme (hex <|> dec)

comma :: Parser String
comma = symbol ","

semi :: Parser String
semi = symbol ";"

equal :: Parser String
equal = symbol "="

delim :: Char -> Parser a -> Parser a
delim s = between (symbol [s]) (symbol [s])

stringlit :: Parser String
stringlit = delim '"' (many $ satisfy (`notElem` "\""))

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

{- Grammar parsing functions -}

validNodeChar :: Parser Char
validNodeChar = alphaNumChar <|> oneOf ",._+-"

validPropChar :: Parser Char
validPropChar = alphaNumChar <|> validNodeChar <|> oneOf "?#"

validLabelChar :: Parser Char
validLabelChar = alphaNumChar <|> char '_'

{- Identifier parsing -}

nodeident :: Parser String
nodeident = string "/" <|> internal
    where internal = (:) <$> letterChar <*> many validNodeChar

refident :: Parser String
refident = (:) <$> char '&' <*> some validLabelChar

defident :: Parser String
defident = (:) <$> (letterChar <|> char '_') <*> many validLabelChar

unitaddr :: Parser String
unitaddr = (:) <$> char '@' <*> many hexDigitChar

nodelabel :: Parser String
nodelabel = (lexeme . try) $ some validLabelChar <* symbol ":"

nodename :: Parser String
nodename = (lexeme . try) $ refident <|> name
  where
    name    = (++) <$> nodeident <*> optaddr
    optaddr = option "" $ try unitaddr

{- Property parsing -}

propname :: Parser String
propname = (lexeme . try) (many validPropChar)

u32 :: Parser Value
u32 = U32 <$> number

str :: Parser Value
str = Str <$> stringlit

ref :: Parser Value
ref = Ref <$> refident

def :: Parser Value
def = Def <$> defident

array :: Parser Value
array = Array <$> between (symbol "<") (symbol ">") content
    where content = (many . lexeme) $ u32 <|> str <|> ref <|> def

propval :: Parser Value
propval = f <$> sepBy1 allowed comma
  where
    f l = if length l == 1 then head l else List l
    allowed = str <|> ref <|> array

boolprop :: Parser Property
boolprop = do
    n <- propname <* semi
    return $ Property n Empty

binprop :: Parser Property
binprop = do
    n <- propname <* equal
    v <- propval <* semi
    return $ Property n v

prop :: Parser Stmt
prop = P <$> (try boolprop <|> try binprop)

{- Node parsing -}

node' :: Parser Stmt
node' = do
    l <- optional nodelabel
    n <- nodename
    c <- braces . many $ (prop <|> node')
    semi
    return . N $ Node l n c

node :: Parser Node
node = do
    N n <- node'
    return n

{- Public interface -}

parseDTS :: Parser [Node]
parseDTS = between sc eof (many node)
