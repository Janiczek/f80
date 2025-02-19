module F80.AST exposing
    ( Program
    , Decl(..)
    , GlobalDeclData, FnDeclData
    , Param
    , Block
    , Stmt(..), WaitForKeyboardItem, CallData, IfStmtData, AssignData
    , DefineConstData, DefineLetData
    , Expr(..), BinOp(..), IfExprData, BinOpData
    , Value(..)
    , KeyPattern(..)
    )

{-|

@docs Program

@docs Decl
@docs GlobalDeclData, FnDeclData

@docs Param
@docs Block
@docs Stmt, WaitForKeyboardItem, CallData, IfStmtData, AssignData
@docs DefineConstData, DefineLetData
@docs Expr, BinOp, IfExprData, BinOpData
@docs Value
@docs KeyPattern

-}


type alias Program =
    List Decl


type Decl
    = GlobalDecl GlobalDeclData
    | FnDecl FnDeclData


type alias GlobalDeclData =
    { name : String
    , value : Value
    }


type Value
    = VGlobal String
    | VInt Int
    | VString String
    | VBytes (List Int)
    | VBinOp VBinOpData
    | VStringLength Value


type alias VBinOpData =
    { left : Value
    , op : BinOp
    , right : Value
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
