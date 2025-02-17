module SSA exposing (Error, Program, translate)

{-| SSA: Static Single Assignment form

The lowered representation of the program where every variable is assigned
exactly once, accounting for shadowing, moving, loops and conditionals (?).

In pseudocode:

    function foo(x):
        let y = (x + 5) * 2
        let z = x - 1
        y += z * 3
        return y

becomes:

    function foo(x):
        let t1 = x + 5
        let y = t1 * 2
        let z = x - 1
        let t2 = z * 3
        let y2 = y + t2
        return y2

Original paper:
<https://www.cs.utexas.edu/~pingali/CS380C/2010/papers/ssaCytron.pdf>

Better paper:
<https://c9x.me/compile/bib/braun13cc.pdf>

Possible other paper:
<https://dl.acm.org/doi/pdf/10.1145/197320.197331>

-}

import AST


type Error
    = TodoSSAError


translate : AST.Program -> Program
translate program =
    Debug.todo "SSA.translate"



----


type alias Program =
    List Decl


type Decl
    = ConstDecl ConstDeclData
    | FnDecl FnDeclData


type alias ConstDeclData =
    { name : String
    , expr : Expr
    }


type alias FnDeclData =
    { name : String
    , params : List Param
    , body : Block
    }


type alias Param =
    String


type alias Block =
    List Stmt


{-| Notably, AST.Assign statement is missing.
-}
type Stmt
    = WaitForKeyboard (List WaitForKeyboardItem)
    | Loop Block
    | If IfStmtData
    | DefineConst DefineConstData
    | DefineLet DefineLetData
    | CallStmt CallData
    | Return (Maybe Expr)


type alias DefineConstData =
    { name : String
    , value : Expr
    }


type alias DefineLetData =
    { name : String
    , value : Expr
    }


type alias IfStmtData =
    { cond : Expr
    , then_ : Block
    , else_ : Maybe Block
    }


type alias CallData =
    { fn : Expr
    , args : List Expr
    }


type alias WaitForKeyboardItem =
    { on : KeyPattern
    , body : Block
    }


type Expr
    = Var String
    | Int Int
    | String String
    | BinOp BinOpData
    | CallExpr CallData
    | IfExpr IfExprData


type alias BinOpData =
    { left : Expr
    , op : BinOp
    , right : Expr
    }


type alias IfExprData =
    { cond : Expr
    , then_ : Expr
    , else_ : Expr
    }


type BinOp
    = BOp_Add
    | BOp_Sub
    | BOp_Gt
    | BOp_Lt


type KeyPattern
    = KeyPattern_Plus
    | KeyPattern_Minus
