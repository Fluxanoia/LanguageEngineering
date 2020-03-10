{
module UsqlPar where

import Absyn
import UsqlLex
}

%name usqlParser
%tokentype  { UsqlLex.Token }
%error      { parseError }

%left '-' '+' 
%left '*' '/'

%token  
        and     { TokenAnd              }
        false   { TokenBool False       }
        true    { TokenBool True        }
        or      { TokenOr               }
        not     { TokenNot              }
        from    { TokenFrom             }
        num     { TokenNum $$           }
        name    { TokenName $$          }
        string  { TokenString $$        }
        select  { TokenSelect           }
        '='     { TokenEq               }
        '+'     { TokenAdd              }
        '-'     { TokenSub              }
        '/'     { TokenDiv              }
        '%'     { TokenMod              }
        '*'     { TokenMul              }
        "<>"    { TokenNE               }
        '<'     { TokenLT               }
        '>'     { TokenGT               }
        ">="    { TokenGE               }
        "<="    { TokenLE               }
        '('     { TokenLPar             }
        ')'     { TokenRPar             }
        ','     { TokenComma            }
        '.'     { TokenDot              }

%%

Main    : Stmt                          { $1 }

Stmt    : select Exprs1 from Names1     { Select $2 $4          }

Names1  : name                          { [$1]                  } 
        | name ',' Names1               { ($1:$3)               }

Column  : name                          { Column $1             }
        | name '.' name                 { TableColumn $1 $3     }

Expr    : Column                        { ColumnExpr $1         }
        | name '(' Exprs ')'            { Prim $1 $3            }
        | Const                         { Cst $1                }
        | '(' Expr ')'                  { $2                    }
        | not Expr                      { Prim "!" [$2]         }
        | Expr '+' Expr                 { Prim "+" [$1, $3]     }
        | Expr '-' Expr                 { Prim "-" [$1, $3]     }
        | Expr '*' Expr                 { Prim "*" [$1, $3]     }
        | Expr '/' Expr                 { Prim "/" [$1, $3]     }
        | Expr '%' Expr                 { Prim "%" [$1, $3]     }
        | Expr '=' Expr                 { Prim "==" [$1, $3]    }
        | Expr "<>" Expr                { Prim "<>" [$1, $3]    }
        | Expr '>' Expr                 { Prim ">" [$1, $3]     }
        | Expr '<' Expr                 { Prim "<" [$1, $3]     }
        | Expr ">=" Expr                { Prim ">=" [$1, $3]    }
        | Expr "<=" Expr                { Prim "<=" [$1, $3]    }
        | Expr and Expr                 { Prim "&&" [$1, $3]    }
        | Expr or Expr                  { Prim "||" [$1, $3]    }
        
Exprs   : {- empty -}                   { []                    }
        | Exprs1                        { $1                    }

Exprs1  : Expr                          { [$1]                  }
        | Expr ',' Exprs1               { ($1:$3)               }

Const   : num                           { CstI $1               }
        | '-' num                       { CstI (-$2)            }
        | false                         { CstB False            }
        | true                          { CstB True             }
        | string                        { CstS $1               }

{

parseError :: [Token] -> a 
parseError _ = error "Parse error"

}