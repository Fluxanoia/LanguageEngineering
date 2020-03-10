{- # Abstract syntax for micro-SQL, very simple SQL SELECT statements # -}

module Absyn where


data Constant = CstI Int                    -- Integer constant
              | CstB Bool                   -- Boolean constant
              | CstS String                 -- String constant
              deriving Show

data Stmt     = Select [Expr] [String]      -- Select statement, where fields are expressions FROM ...
              deriving Show

data Column   = Column String               -- A column name: c
              | TableColumn String String   -- A qualified column: t.c
              deriving Show

data Expr     = Star 
              | Cst Constant                -- Constant
              | ColumnExpr Column           -- Column
              | Prim String [Expr]          -- Built-in function
              deriving Show