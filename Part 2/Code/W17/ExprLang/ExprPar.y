{
module ExprPar where

import Absyn
import ExprLex
}

%name exprParser
%tokentype  { ExprLex.Token }
%error      { parseError }
%left '-' '+' 
%left '*'

%token 
        let     { TokenLet  }
        in      { TokenIn   }
        end     { TokenEnd  }
        num     { TokenNum $$ }
        var     { TokenVar $$ }
        '='     { TokenEq   }
        '+'     { TokenAdd  }
        '-'     { TokenSub  }
        '*'     { TokenMul  }
        '('     { TokenLPar }
        ')'     { TokenRPar }

%%

Expr    : var                          { Var  $1           }
        | num                          { CstI $1           }
        | '-' num                      { CstI (- $2)       }
        | '(' Expr ')'                 { $2                }
        | let var '=' Expr in Expr end { Let $2 $4 $6      }
        | Expr '*' Expr                { Prim "*" $1 $3    }
        | Expr '+' Expr                { Prim "+" $1 $3    }
        | Expr '-' Expr                { Prim "-" $1 $3    }
        
{

parseError :: [Token] -> a 
parseError _ = error "Parse error"

}