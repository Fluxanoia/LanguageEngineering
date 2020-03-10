{
module FunPar where

import Absyn
import FunLex
}

%name funParser
%tokentype  { FunLex.Token }
%error      { parseError }


%token  
        else    { TokenElse             }
        end     { TokenEnd              }
        bool    { TokenBool $$          }
        if      { TokenIf               }
        in      { TokenIn               }
        let     { TokenLet              }
        not     { TokenNot              }
        then    { TokenThen             }
        num     { TokenNum $$           }
        name    { TokenName $$          }
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
        eof     { TokenEOF              }
%%

Main    : Expr                                  { $1 }

Expr    : AtExpr                                { $1                    }
        | AppExpr                               { $1                    }
        | if Expr then Expr else Expr           { If $2 $4 $6           }
        | '-' Expr                              { Prim "-" (CstI 0) $2  }
        | Expr '+' Expr                         { Prim "+" $1 $3        }
        | Expr '-' Expr                         { Prim "-" $1 $3        }
        | Expr '*' Expr                         { Prim "*" $1 $3        }
        | Expr '/' Expr                         { Prim "/" $1 $3        }
        | Expr '%' Expr                         { Prim "%" $1 $3        }
        | Expr '=' Expr                         { Prim "=" $1 $3        }
        | Expr "<>" Expr                        { Prim "<>" $1 $3       }
        | Expr '>' Expr                         { Prim ">" $1 $3        }
        | Expr '<' Expr                         { Prim "<" $1 $3        }
        | Expr ">=" Expr                        { Prim ">=" $1 $3       }
        | Expr "<=" Expr                        { Prim "<=" $1 $3       }

AtExpr  : Const                                 { $1                    }
        | name                                  { Var $1                }
        | let name '=' Expr in Expr end         { Let $2 $4 $6          }
        | let name name '=' Expr in Expr end    { Letfun $2 $3 $5 $7    }
        | '(' Expr ')'                          { $2                    }

AppExpr : AtExpr AtExpr                         { Call $1 $2            }
        | AppExpr AtExpr                        { Call $1 $2            }

Const   : num                                   { CstI $1               }
        | bool                                  { CstB $1               }

{

parseError :: [Token] -> a 
parseError _ = error "Parse error"

}