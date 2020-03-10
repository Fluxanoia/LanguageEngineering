{
module UsqlLex (  usqlLexer
                , Token(..)) where 
}

%wrapper "basic"

$alpha      = [a-zA-Z]
$alphanum   = [a-zA-z0-9]
$digit      = 0-9
$graphic    = $printable # $white

@string     = \' ($graphic # \')* \'

tokens :-

    $white+                                     ;
    $digit+                                     { \s -> TokenNum (read s)  }
    (\<\>|\<\=|\>\=)                            { \s -> let (x:y:ys) = s in operator [x,y]}
    (\+|\-|\*|\/|\%|\>|\<|\=)                   { \s -> operator ([head s])  }
    (\(|\)|\,|\.)                               { \s -> delimiter ([head s]) }
    \-\- $graphic "\n"                          ;
    @string                                     { \s -> TokenString s}
    IN                                          { \s -> TokenIn }
    AND                                         { \s -> TokenAnd}
    false                                       { \s -> TokenBool False}
    true                                        { \s -> TokenBool True}
    SELECT                                      { \s -> TokenSelect}
    OR                                          { \s -> TokenOr}
    FROM                                        { \s -> TokenFrom}
    NOT                                         { \s -> TokenNot}
    $alpha [$alphanum \_]*                      { \s -> TokenName s }

{

operator :: String -> Token 
operator c = case c of "+"   -> TokenAdd 
                       "-"   -> TokenSub 
                       "*"   -> TokenMul
                       "/"   -> TokenDiv 
                       "%"   -> TokenMod
                       "="   -> TokenEq
                       "<>"  -> TokenNE
                       ">"   -> TokenGT
                       "<"   -> TokenLT 
                       ">="  -> TokenGE 
                       "<="  -> TokenLE 


delimiter :: String -> Token 
delimiter c = case c of "("   -> TokenLPar
                        ")"   -> TokenRPar 
                        ","   -> TokenComma
                        "."   -> TokenDot 

data Token  = TokenIn 
            | TokenEnd 
            | TokenName String
            | TokenNum Int
            | TokenAdd
            | TokenSub
            | TokenMul
            | TokenDiv
            | TokenEq 
            | TokenLPar 
            | TokenRPar
            | TokenMod 
            | TokenNE 
            | TokenGT 
            | TokenLT 
            | TokenGE 
            | TokenLE 
            | TokenDot 
            | TokenComma 
            | TokenString String
            | TokenAnd 
            | TokenBool Bool
            | TokenOr 
            | TokenFrom 
            | TokenSelect
            | TokenNot
            deriving (Show, Eq)

usqlLexer :: String -> [Token]
usqlLexer s = alexScanTokens s
}