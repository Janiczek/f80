module F80.Emitter exposing (emit)

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


emit : Program -> List String
emit program =
    program
        |> List.map emitDecl
        |> List.concat


emitDecl : Decl -> List String
emitDecl decl =
    case decl of
        GlobalDecl data ->
            emitGlobalDecl data

        FnDecl data ->
            emitFnDecl data


emitGlobalDecl : GlobalDeclData -> List String
emitGlobalDecl globalData =
    let
        default () =
            [ globalData.name ++ " db " ++ emitValue globalData.value ]
    in
    case globalData.value of
        VInt _ ->
            default ()

        VBytes _ ->
            default ()

        VBinOp _ ->
            default ()

        VStringLength _ ->
            default ()

        VString s ->
            -- Also make a label for the end of the string so that we can compute the length
            -- TODO check for off-by-one
            [ globalData.name ++ " db " ++ emitValue globalData.value
            , globalStringLengthLabel globalData.name ++ " EQU " ++ String.fromInt (String.length s)
            ]

        VGlobal otherName ->
            -- Use EQU so that we don't allocate the same data multiple times
            [ globalData.name ++ " EQU " ++ otherName ]


emitValue : Value -> String
emitValue val =
    case val of
        VInt i ->
            String.fromInt i

        VString s ->
            "'" ++ s ++ "', 0"

        VBytes bytes ->
            bytes
                |> List.map String.fromInt
                |> String.join ", "

        VGlobal otherName ->
            otherName

        VBinOp binOpData ->
            emitValue binOpData.left
                ++ " "
                ++ emitBinOp binOpData.op
                ++ " "
                ++ emitValue binOpData.right

        VStringLength val_ ->
            case val_ of
                VString s ->
                    emitValue (VInt (String.length s))

                VGlobal otherName ->
                    globalStringLengthLabel otherName

                _ ->
                    -- TODO exit with an error?
                    "ERROR: String length used on a non-string/global value"


globalStringLengthLabel : String -> String
globalStringLengthLabel name =
    name ++ "_length"


emitBinOp : BinOp -> String
emitBinOp op =
    case op of
        BOp_Add ->
            "+"

        BOp_Sub ->
            "-"

        BOp_Lt ->
            "<"

        BOp_Gt ->
            ">"


{-| Emit a function definition.
-}
emitFnDecl : FnDeclData -> List String
emitFnDecl fnData =
    let
        bodyLines : List String
        bodyLines =
            fnData.body
                |> List.map emitStmt
                |> List.concat
    in
    [ "; function " ++ fnData.name
    , "_" ++ fnData.name ++ ":"
    ]
        ++ bodyLines
        ++ [ "ret" ]


emitStmt : Stmt -> List String
emitStmt stmt =
    case stmt of
        WaitForKeyboard _ ->
            [ "; WaitForKeyboard not implemented" ]

        Loop subBlock ->
            [ "; begin loop" ]
                ++ (subBlock
                        |> List.map emitStmt
                        |> List.concat
                   )
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


{-| Emit a function call statement.
-}
emitCall : CallData -> List String
emitCall callData =
    let
        numArgs : Int
        numArgs =
            List.length callData.args
    in
    pushArgs callData.args
        ++ callInstruction callData.fn
        ++ cleanupStack numArgs


{-| Push function arguments onto the stack from left to right.
-}
pushArgs : List Expr -> List String
pushArgs args =
    args
        |> List.map (\arg -> emitExprToHL arg ++ [ "push hl" ])
        |> List.concat


callInstruction : Expr -> List String
callInstruction fnExpr =
    case fnExpr of
        Var fnName ->
            [ "call _" ++ fnName ]

        _ ->
            [ "; function expression not supported in call" ]


cleanupStack : Int -> List String
cleanupStack numArgs =
    if numArgs > 0 then
        [ "add sp," ++ String.fromInt (numArgs * 2) ]

    else
        []


emitExprToHL : Expr -> List String
emitExprToHL expr =
    case expr of
        Int i ->
            [ "ld hl," ++ String.fromInt i ]

        Var name ->
            [ "; load a global variable (address in RAM) into HL"
            , "ld hl,(" ++ name ++ ")"
            ]

        _ ->
            [ "; TODO: handle " ++ Debug.toString expr ]
