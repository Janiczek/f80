module F80.AST exposing
    ( Program
    , Decl(..)
    , GlobalDeclData, FnDeclData
    , Param
    , Block, walkBlock
    , Stmt(..), walkStmt
    , WaitForKeypressItem, CallData, IfStmtData, AssignData
    , DefineConstData, DefineLetData
    , Expr(..), walkExpr
    , IfExprData
    , BinOp(..), BinOpData
    , UnaryOp(..), UnaryOpData
    , Value(..)
    , KeyPattern(..), keyPatternName
    , mainFnName
    )

{-|

@docs Program

@docs Decl
@docs GlobalDeclData, FnDeclData

@docs Param
@docs Block, walkBlock
@docs Stmt, walkStmt
@docs WaitForKeypressItem, CallData, IfStmtData, AssignData
@docs DefineConstData, DefineLetData
@docs Expr, walkExpr
@docs IfExprData
@docs BinOp, BinOpData
@docs UnaryOp, UnaryOpData
@docs Value
@docs KeyPattern, keyPatternName
@docs mainFnName

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
    | VBool Bool -- Represented as 0 / 255.
    | VBytes (List Int)
    | VBinOp VBinOpData
    | VUnaryOp VUnaryOpData
    | VStringLength Value


type alias VBinOpData =
    { left : Value
    , op : BinOp
    , right : Value
    }


type alias VUnaryOpData =
    { op : UnaryOp
    , expr : Value
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
    = WaitForKeypress (List WaitForKeypressItem)
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
    { fn : String
    , args : List Expr
    }


type alias WaitForKeypressItem =
    { on : KeyPattern
    , body : Block
    }


type Expr
    = Var String
    | Int Int
    | String String
    | Bool Bool -- Represented as 0 / 255.
    | BinOp BinOpData
    | UnaryOp UnaryOpData
    | CallExpr CallData
    | IfExpr IfExprData


type alias BinOpData =
    { left : Expr
    , op : BinOp
    , right : Expr
    }


type alias UnaryOpData =
    { op : UnaryOp
    , expr : Expr
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


type UnaryOp
    = UOp_Neg
    | UOp_Not


type KeyPattern
    = KeyPattern_J
    | KeyPattern_K


keyPatternName : KeyPattern -> String
keyPatternName keyPattern =
    case keyPattern of
        KeyPattern_J ->
            "J"

        KeyPattern_K ->
            "K"


walkStmt :
    (acc -> Expr -> ( acc, Expr ))
    -> (acc -> Stmt -> ( acc, Stmt ))
    -> acc
    -> Stmt
    -> ( acc, Stmt )
walkStmt fExpr fStmt acc stmt =
    let
        ( newAcc, newStmt ) =
            fStmt acc stmt
    in
    case newStmt of
        WaitForKeypress items ->
            let
                ( finalAcc, newItems ) =
                    List.foldl
                        (\item ( accItem, items_ ) ->
                            let
                                ( newAccItem, newBody ) =
                                    walkBlock fExpr fStmt accItem item.body
                            in
                            ( newAccItem
                            , items_ ++ [ { item | body = newBody } ]
                            )
                        )
                        ( newAcc, [] )
                        items
            in
            ( finalAcc, WaitForKeypress newItems )

        Loop block ->
            let
                ( newAcc1, newBlock ) =
                    walkBlock fExpr fStmt newAcc block
            in
            ( newAcc1, Loop newBlock )

        If data ->
            let
                ( newAcc1, newCond ) =
                    walkExpr fExpr newAcc data.cond

                ( newAcc2, newThen ) =
                    walkBlock fExpr fStmt newAcc1 data.then_

                ( newAcc3, newElse ) =
                    case data.else_ of
                        Nothing ->
                            ( newAcc2, Nothing )

                        Just else_ ->
                            walkBlock fExpr fStmt newAcc2 else_
                                |> Tuple.mapSecond Just
            in
            ( newAcc3
            , If
                { data
                    | cond = newCond
                    , then_ = newThen
                    , else_ = newElse
                }
            )

        DefineConst data ->
            let
                ( newAcc1, newExpr ) =
                    walkExpr fExpr newAcc data.value
            in
            ( newAcc1, DefineConst { data | value = newExpr } )

        DefineLet data ->
            let
                ( newAcc1, newExpr ) =
                    walkExpr fExpr newAcc data.value
            in
            ( newAcc1, DefineLet { data | value = newExpr } )

        Assign assignData ->
            let
                ( newAcc1, newExpr ) =
                    walkExpr fExpr newAcc assignData.value
            in
            ( newAcc1, Assign { assignData | value = newExpr } )

        CallStmt data ->
            let
                ( newAcc1, newArgs ) =
                    List.foldl
                        (\arg ( accArg, args ) ->
                            let
                                ( newAccArg, newArg ) =
                                    walkExpr fExpr accArg arg
                            in
                            ( newAccArg, args ++ [ newArg ] )
                        )
                        ( newAcc, [] )
                        data.args
            in
            ( newAcc1, CallStmt { data | args = newArgs } )

        Return maybeExpr ->
            case maybeExpr of
                Nothing ->
                    ( newAcc, Return Nothing )

                Just expr ->
                    let
                        ( newAcc1, newExpr ) =
                            walkExpr fExpr newAcc expr
                    in
                    ( newAcc1, Return (Just newExpr) )


walkBlock :
    (acc -> Expr -> ( acc, Expr ))
    -> (acc -> Stmt -> ( acc, Stmt ))
    -> acc
    -> List Stmt
    -> ( acc, List Stmt )
walkBlock fExpr fStmt acc stmts =
    List.foldl
        (\stmt ( acc1, stmts1 ) ->
            let
                ( acc2, stmt1 ) =
                    walkStmt fExpr fStmt acc1 stmt
            in
            ( acc2, stmt1 :: stmts1 )
        )
        ( acc, [] )
        stmts
        |> Tuple.mapSecond List.reverse


walkExpr :
    (acc -> Expr -> ( acc, Expr ))
    -> acc
    -> Expr
    -> ( acc, Expr )
walkExpr f acc expr =
    let
        ( newAcc, newExpr ) =
            f acc expr
    in
    case newExpr of
        BinOp data ->
            let
                ( accLeft, newLeft ) =
                    walkExpr f newAcc data.left

                ( accRight, newRight ) =
                    walkExpr f accLeft data.right
            in
            ( accRight
            , BinOp { data | left = newLeft, right = newRight }
            )

        UnaryOp data ->
            let
                ( accExpr, newArg ) =
                    walkExpr f newAcc data.expr
            in
            ( accExpr, UnaryOp { data | expr = newArg } )

        IfExpr data ->
            let
                ( accCond, newCond ) =
                    walkExpr f newAcc data.cond

                ( accThen, newThen ) =
                    walkExpr f accCond data.then_

                ( accElse, newElse ) =
                    walkExpr f accThen data.else_
            in
            ( accElse
            , IfExpr { data | cond = newCond, then_ = newThen, else_ = newElse }
            )

        CallExpr data ->
            let
                ( finalAcc, newArgs ) =
                    List.foldl
                        (\arg ( accArg, args ) ->
                            let
                                ( newAccArg, newArg ) =
                                    walkExpr f accArg arg
                            in
                            ( newAccArg, args ++ [ newArg ] )
                        )
                        ( newAcc, [] )
                        data.args
            in
            ( finalAcc
            , CallExpr { data | args = newArgs }
            )

        Var _ ->
            ( newAcc, newExpr )

        Int _ ->
            ( newAcc, newExpr )

        Bool _ ->
            ( newAcc, newExpr )

        String _ ->
            ( newAcc, newExpr )


mainFnName : String
mainFnName =
    "main"
