module F80.Emitter exposing (emit)

import F80.AST
    exposing
        ( AssignData
        , Block
        , CallData
        , ConstDeclData
        , Decl(..)
        , Expr(..)
        , FnDeclData
        , IfStmtData
        , Stmt(..)
        )


emit : F80.AST.Program -> List String
emit program =
    let
        emitDecl : Decl -> List String
        emitDecl decl =
            case decl of
                ConstDecl data ->
                    emitConstDecl data

                FnDecl data ->
                    emitFnDecl data

        emitConstDecl : ConstDeclData -> List String
        emitConstDecl constData =
            case constData.expr of
                Int i ->
                    [ constData.name ++ " EQU " ++ String.fromInt i ]

                _ ->
                    [ "; TODO: More sophisticated constant expression handling"
                    , "; " ++ constData.name
                    ]

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
            let
                -- A small helper to load an expression's value into HL.
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

                -- Push each function argument onto the stack from left to right.
                pushArgs : List String
                pushArgs =
                    callData.args
                        |> List.map (\arg -> emitExprToHL arg ++ [ "push hl" ])
                        |> List.concat

                -- Perform the actual call, if it's a Var <fnName>.
                callInstruction : List String
                callInstruction =
                    case callData.fn of
                        Var fnName ->
                            [ "call _" ++ fnName ]

                        _ ->
                            [ "; function expression not supported in call" ]

                -- After the call, clean up the stack by adding (number_of_arguments * 2).
                cleanupStack : List String
                cleanupStack =
                    let
                        numArgs : Int
                        numArgs =
                            List.length callData.args
                    in
                    if numArgs > 0 then
                        [ "add sp," ++ String.fromInt (numArgs * 2) ]

                    else
                        []
            in
            pushArgs
                ++ callInstruction
                ++ cleanupStack

        emitBlock : Block -> List String
        emitBlock block =
            List.concatMap emitStmt block
    in
    List.concatMap emitDecl program
