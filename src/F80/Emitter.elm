module F80.Emitter exposing (emit)

{-| Prepares assembly for consumption by the PASMO Z80 assembler.
-}

import F80.AST
    exposing
        ( AssignData
        , BinOp(..)
        , Block
        , CallData
        , Decl(..)
        , Expr(..)
        , FnDeclData
        , GlobalDeclData
        , IfStmtData
        , Program
        , Stmt(..)
        , Value(..)
        )
import F80.Emitter.Global
import F80.Emitter.Util exposing (i, l)


emit : Program -> List String
emit program =
    List.concatMap emitDecl program


emitDecl : Decl -> List String
emitDecl decl =
    case decl of
        GlobalDecl data ->
            F80.Emitter.Global.emitGlobalDecl data

        FnDecl data ->
            emitFnDecl data


emitFnDecl : FnDeclData -> List String
emitFnDecl fnData =
    List.concat
        [ [ l fnData.name ]
        , emitBlock fnData.body
        , [ i "ret" ]
        ]


emitStmt : Stmt -> List String
emitStmt stmt =
    case stmt of
        WaitForKeyboard _ ->
            Debug.todo "emitStmt WaitForKeyboard"

        Loop subBlock ->
            [ "; begin loop" ]
                ++ emitBlock subBlock
                ++ [ "; end loop" ]

        If ifData ->
            emitIf ifData

        DefineConst defConst ->
            [ "; define const " ++ defConst.name
            , "; (not directly implemented in code generation)"
            ]

        DefineLet defLet ->
            [ "; define let " ++ defLet.name
            , "; (not directly implemented in code generation)"
            ]

        Assign assignData ->
            emitAssign assignData

        CallStmt callData ->
            emitCall callData


emitIf : IfStmtData -> List String
emitIf ifData =
    [ "; if statement (cond not fully implemented yet)" ]
        ++ emitBlock ifData.then_
        ++ (case ifData.else_ of
                Nothing ->
                    []

                Just elseBlock ->
                    [ "; else" ]
                        ++ emitBlock elseBlock
           )


emitBlock : Block -> List String
emitBlock block =
    List.concatMap emitStmt block


emitAssign : AssignData -> List String
emitAssign assignData =
    let
        opComment : String
        opComment =
            case assignData.op of
                Nothing ->
                    ""

                Just op ->
                    " ; operator: " ++ Debug.toString op
    in
    [ "; assign " ++ assignData.var ++ opComment
    , "; (not fully implemented)"
    ]


emitCall : CallData -> List String
emitCall callData =
    List.concat
        [ pushArgs callData.args
        , [ i <| "call " ++ callData.fn ]
        ]


pushArgs : List Expr -> List String
pushArgs args =
    List.concatMap
        (\arg -> emitExprToHL arg ++ [ i "push hl" ])
        args


emitExprToHL : Expr -> List String
emitExprToHL expr =
    case expr of
        Int n ->
            [ i <| "ld hl," ++ String.fromInt n ]

        Var name ->
            -- TODO this will only work for globals, but not for locals
            [ i <| "ld hl,(" ++ name ++ ")"
            ]

        _ ->
            Debug.todo <| "emit expr to HL: " ++ Debug.toString expr
