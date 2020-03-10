module Parse where

import Absyn
import UsqlPar 
import UsqlLex

-- | Call generated expr lexer and parser on input string
-- | > ghci Parse.hs Absyn.hs UsqlLex.hs UsqlPar.hs

parseFromString :: String -> Stmt
parseFromString s = (usqlParser . usqlLexer) s

parseFromFile :: String -> IO ()
parseFromFile filename = do 
    s <- readFile filename
    print $ parseFromString s 
