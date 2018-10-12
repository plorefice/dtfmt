module AST
    ( Property(..)
    , Value(..)
    , Label(..)
    , Stmt(..)
    , Node(..)
    , Include(..)
    , Source(..)
    , parseSource
    )
where

import           Data.List
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

data Property = Property String Value deriving (Eq)

data Value
    = Empty
    | U32 String
    | Str String
    | Ref String
    | Def String
    | List [Value]
    | Array [Value]
    deriving (Eq)

type Label = String

data Include
    = Local String
    | Global String
    deriving (Eq)

data Stmt
    = N Node
    | P Property
    deriving (Eq)

data Node = Node (Maybe Label) String [Stmt] deriving (Eq)

data Source = Source [Include] [Node] deriving (Eq)

{- Show implementations -}

instance Show Value where
    show Empty     = ""
    show (U32 u )  = u
    show (Str s )  = "\"" ++ s ++ "\""
    show (Ref r )  = r
    show (Def d )  = d
    show (List l)  = intercalate ", " $ fmap show l
    show (Array l) = "<" ++ unwords (fmap show l) ++ ">"

instance Show Property where
    show (Property s Empty) = s ++ ";"
    show (Property s v) = s ++ " = " ++ show v ++ ";"

instance Show Node where
    show (Node (Just l) n ss) = l ++ ": " ++ n ++ " {\n" ++ f ss ++ "};"
        where f = concatMap ((++ "\n") . show)

    show (Node Nothing n ss) = n ++ " {\n" ++ f ss ++ "};"
        where f = concatMap ((++ "\n") . show)

instance Show Stmt where
    show (N n) = show n
    show (P p) = show p

instance Show Include where
    show (Local s) = "#include \"" ++ s ++ "\""
    show (Global s) = "#include <" ++ s ++ ">"

instance Show Source where
    show (Source incs ns) = f incs ++ "\n\n" ++ f ns
        where f xs = intercalate "\n" $ fmap show xs

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

validPathChar :: Parser Char
validPathChar = alphaNumChar <|> oneOf "._-/"

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

propname :: Parser String
propname = (lexeme . try) (many validPropChar)

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

{- Directives parsing -}

include :: Parser Include
include = lexeme (string "#include") *> (local <|> global)
  where
    local  = Local <$> delim '"' (some validPathChar)
    global = Global <$> between (symbol "<") (symbol ">") (some validPathChar)

{- Public interface -}

parseSource :: Parser Source
parseSource = between sc eof $ do
    incs <- many include
    ns   <- many node
    return $ Source incs ns
