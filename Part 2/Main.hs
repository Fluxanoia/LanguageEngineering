import Wk14
import Wk16

let_expr :: Expr
let_expr = Let [("x", Prim Plus (Var "x") (Integer 7))] (Prim Plus (Var "x") (Integer 8))
