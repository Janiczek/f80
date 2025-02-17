module AST exposing
    ( Program
    , Decl(..)
    , ConstDeclData, FnDeclData
    , Param
    , Block
    , Stmt(..), WaitForKeyboardItem, CallData, IfStmtData, AssignData
    , DefineConstData, DefineLetData
    , Expr(..), BinOp(..), IfExprData, BinOpData
    , KeyPattern(..)
    )

{-| The front-end syntax of the language.

@docs Program

@docs Decl
@docs ConstDeclData, FnDeclData

@docs Param
@docs Block
@docs Stmt, WaitForKeyboardItem, CallData, IfStmtData, AssignData
@docs DefineConstData, DefineLetData
@docs Expr, BinOp, IfExprData, BinOpData
@docs KeyPattern

-}


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


type Stmt
    = WaitForKeyboard (List WaitForKeyboardItem)
    | Loop Block
    | If IfStmtData
    | DefineConst DefineConstData
    | DefineLet DefineLetData
    | Assign AssignData
    | CallStmt CallData
    | Return (Maybe Expr)


type alias AssignData =
    { var : String
    , op : Maybe BinOp
    , value : Expr
    }


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
