{
module FunLex (   funLexer
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
    (\(|\))                                     { \s -> delimiter ([head s]) }
    \(\* $printable \*\)                        ;
    else                                        { \s -> TokenElse }
    end                                         { \s -> TokenEnd }
    false                                       { \s -> TokenBool False}
    true                                        { \s -> TokenBool True}
    if                                          { \s -> TokenIf }
    in                                          { \s -> TokenIn }
    let                                         { \s -> TokenLet }
    not                                         { \s -> TokenNot}
    then                                        { \s -> TokenThen }
    eof                                         { \s -> TokenEOF }
    $alpha [$alphanum]*                         { \s -> TokenName s }

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

data Token  = TokenLet 
            | TokenIn 
            | TokenIf 
            | TokenElse 
            | TokenBool Bool 
            | TokenNot 
            | TokenEnd 
            | TokenEOF
            | TokenThen
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
            deriving (Show, Eq)

funLexer :: String -> [Token]
funLexer s = alexScanTokens s
}