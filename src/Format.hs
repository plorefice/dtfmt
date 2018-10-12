module Format
  ( format
  )
where

import           AST
import           Data.List

(==>) :: String -> Int -> String
(==>) s n = concat (replicate n "    ") ++ s

format :: Source -> [String]
format (Source []           []      ) = [""]
format (Source []           (n : ns)) = fmtNode n 0 ++ format (Source [] ns)
format (Source (inc : incs) ns      ) = fmtInclude inc : format (Source incs ns)

fmtInclude :: Include -> String
fmtInclude (Local  p) = "#include \"" ++ p ++ "\""
fmtInclude (Global p) = "#include <" ++ p ++ ">"

fmtLabel :: Maybe String -> String
fmtLabel (Just l) = l ++ ": "
fmtLabel Nothing  = ""

fmtNode :: Node -> Int -> [String]
fmtNode (Node label name ss) ind = "" : open : content ss ++ close
 where
  open    = (fmtLabel label ++ name ++ " {") ==> ind
  content = concatMap (\s -> fmtStmt s (ind + 1))
  close   = ["};" ==> ind]

fmtStmt :: Stmt -> Int -> [String]
fmtStmt (N node) ind = fmtNode node ind
fmtStmt (P prop) ind = fmtProp prop ind

fmtProp :: Property -> Int -> [String]
fmtProp (Property s Empty) ind = [(s ++ ";") ==> ind]
fmtProp (Property s v    ) ind = [(s ++ " = " ++ fmtValue v ++ ";") ==> ind]

fmtValue :: Value -> String
fmtValue Empty     = ""
fmtValue (U32   u) = u
fmtValue (Str   s) = "\"" ++ s ++ "\""
fmtValue (Ref   r) = r
fmtValue (Def   d) = d
fmtValue (List  l) = intercalate ", " $ fmap show l
fmtValue (Array l) = "<" ++ unwords (fmap show l) ++ ">"
