module F80.Emitter exposing (emit)

{-| Prepares assembly for consumption by the PASMO Z80 assembler.

Arguments to functions are passed on the stack.
Function return values are in register A.

-}

import Dict exposing (Dict)
import F80.AST as AST
    exposing
        ( AssignData
        , BinOp(..)
        , Block
        , CallData
        , Decl(..)
        , Expr(..)
        , FnDeclData
        , GlobalDeclData
        , IfExprData
        , IfStmtData
        , Program
        , Stmt(..)
        , UnaryOp(..)
        , Value(..)
        , WaitForKeypressItem
        )
import F80.Emitter.Global
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.Util as Util exposing (ctxLabel, i, l)
import F80.Emitter.WaitForKeypress


emit : Program -> List String
emit program =
    program
        |> List.indexedMap emitDecl
        |> List.foldl Output.add Output.empty
        |> Output.toString


emitDecl : Int -> Decl -> Output
emitDecl ix decl =
    case decl of
        GlobalDecl data ->
            F80.Emitter.Global.emit data

        FnDecl data ->
            let
                ctx =
                    [ String.fromInt ix, "decl" ]
            in
            emitFnDecl ctx data


emitFnDecl : List String -> FnDeclData -> Output
emitFnDecl ctx fnData =
    let
        isMain =
            fnData.name == AST.mainFnName

        blockOutput =
            emitBlock { isMain = isMain } (fnData.name :: ctx) fnData.body
    in
    if isMain then
        blockOutput
            |> Output.andThen
                (\blockCode ->
                    Output.code
                        (List.concat
                            [ [ l fnData.name ]
                            , blockCode
                            ]
                        )
                )

    else
        blockOutput
            |> Output.andThen
                (\blockCode ->
                    Output.other
                        (List.concat
                            [ [ l fnData.name ]
                            , blockCode

                            -- all functions have been lowered to explicitly have returns. we don't need to add `ret` here.
                            ]
                        )
                )


{-| These will emit their ASM blocks in the Output.mainCode field. That doesn't mean it's meant to go into `main()`!
-}
emitStmt : { isMain : Bool } -> List String -> Int -> Stmt -> Output
emitStmt isMain parentCtx ix stmt =
    let
        ctx =
            String.fromInt ix :: parentCtx
    in
    case stmt of
        WaitForKeypress data ->
            F80.Emitter.WaitForKeypress.emit ctx (emitBlock isMain) data

        Loop block ->
            let
                label =
                    Util.ctxLabel ctx
            in
            Output.code [ l label ]
                |> Output.add (emitBlock isMain ("loop" :: ctx) block)
                |> Output.add (Output.code [ i <| "jp " ++ label ])

        If ifData ->
            emitIfStmt isMain ctx ifData

        DefineConst defConst ->
            Debug.todo <| "emitStmt defineConst: " ++ Debug.toString defConst

        DefineLet defLet ->
            Debug.todo <| "emitStmt defineLet: " ++ Debug.toString defLet

        Assign assignData ->
            emitAssign assignData

        CallStmt callData ->
            emitCall callData

        Return maybeExpr ->
            emitReturn isMain ctx maybeExpr


emitReturn : { isMain : Bool } -> List String -> Maybe Expr -> Output
emitReturn { isMain } ctx maybeExpr =
    let
        return =
            if isMain then
                Output.code [ i "jp _end" ]

            else
                Output.code [ i "ret" ]
    in
    case maybeExpr of
        Nothing ->
            return

        Just expr ->
            {- We emit the code of the expr, even if the main() return ultimately
               throws it away. That's because we can have side effects in our
               expressions!
            -}
            emitExpr ("return" :: ctx) expr
                |> Output.add return


emitIfStmt : { isMain : Bool } -> List String -> IfStmtData -> Output
emitIfStmt isMain ctx ifData =
    let
        ctxLabel =
            Util.ctxLabel ctx

        labelPrefix =
            "_ifstmt_" ++ ctxLabel ++ "_"

        endLabel =
            labelPrefix ++ "end"
    in
    case ifData.else_ of
        Nothing ->
            emitExpr ("cond" :: ctx) ifData.cond
                |> Output.add
                    (Output.code
                        [ i "cp 255"
                        , i <| "jp nz," ++ endLabel
                        ]
                    )
                |> Output.add (emitBlock isMain ("then" :: ctx) ifData.then_)
                |> Output.add (Output.code [ l endLabel ])

        Just else_ ->
            let
                elseLabel =
                    labelPrefix ++ "else"
            in
            emitExpr ("cond" :: ctx) ifData.cond
                |> Output.add
                    (Output.code
                        [ i "cp 255"
                        , i <| "jp nz," ++ elseLabel
                        ]
                    )
                |> Output.add (emitBlock isMain ("then" :: ctx) ifData.then_)
                |> Output.add
                    (Output.code
                        [ i <| "jp " ++ endLabel
                        , l elseLabel
                        ]
                    )
                |> Output.add (emitBlock isMain ("else" :: ctx) else_)
                |> Output.add (Output.code [ l endLabel ])


emitBlock : { isMain : Bool } -> List String -> Block -> Output
emitBlock isMain ctx block =
    block
        |> List.indexedMap (emitStmt isMain ctx)
        |> List.foldl Output.add Output.empty


emitAssign : AssignData -> Output
emitAssign assignData =
    Debug.todo "emitAssign"


emitCall : CallData -> Output
emitCall callData =
    case callData.fn of
        "ROM.clearScreen" ->
            Output.romCls

        "Render.text" ->
            emitCallRenderText callData

        _ ->
            Output.code
                (List.concat
                    [ pushArgs callData.args

                    -- This will work for global functions, not for lambda exprs. That's OK, we don't have lambdas (yet) :)
                    , [ i <| "call " ++ callData.fn ]
                    ]
                )


{-| Clobbers DE,HL. If that causes issues, TODO clean up after ourselves?
-}
emitCallRenderText : CallData -> Output
emitCallRenderText callData =
    case callData.args of
        [ x, y, string ] ->
            Output.code
                (List.concat
                    [ renderTextXYHL x y
                    , [ case string of
                            Var name ->
                                i <| "ld de," ++ name

                            String str ->
                                Debug.todo "Render.text - this should have been caught - we should have hoisted the string literal to a global constant and changed this call to use the var"

                            _ ->
                                let
                                    _ =
                                        Debug.log "Unexpected Render.text string argument" string
                                in
                                Debug.todo "Unexpected Render.text string argument - this should have been typechecked before emitting"
                      , i <| "call _renderString"
                      ]
                    ]
                )
                |> Output.add Output.renderText

        _ ->
            Debug.todo "emitCallRenderText - unexpected number of arguments"


renderTextXYHL : Expr -> Expr -> List String
renderTextXYHL x y =
    let
        emitInt : String -> Int -> String
        emitInt reg n =
            i <| "ld " ++ reg ++ "," ++ String.fromInt n

        emitVar : String -> String -> String
        emitVar reg var =
            Debug.todo "TODO emit var"
    in
    case ( x, y ) of
        ( Int xx, Int yy ) ->
            -- Special optimized case
            -- ld hl,0xXXYY
            [ i <| "ld hl," ++ String.fromInt (xx * 256 + yy) ]

        ( Int xx, Var yy ) ->
            [ emitInt "h" xx
            , emitVar "l" yy
            ]

        ( Var xx, Int yy ) ->
            [ emitVar "h" xx
            , emitInt "l" yy
            ]

        ( Var xx, Var yy ) ->
            [ emitVar "h" xx
            , emitVar "l" yy
            ]

        ( _, _ ) ->
            let
                _ =
                    Debug.log "Unexpected Render.text X,Y argument" ( x, y )
            in
            Debug.todo "Unexpected Render.text X,Y argument - this should have been typechecked before emitting"


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


{-| Loads the value into the register A.
-}
emitExpr : List String -> Expr -> Output
emitExpr ctx expr =
    case expr of
        Int n ->
            Output.code [ i <| "ld a," ++ String.fromInt n ]

        Var var ->
            -- TODO this is probably too simplistic. We might need to split to
            -- Global and Local vars, or have a way of knowing which one we're
            -- looking at.
            -- The `ld a,var` approach should work for globals since the var is a label.
            Output.code [ i <| "ld a," ++ var ]

        String _ ->
            Debug.todo "emitExpr String - this shouldn't have happened - we hoisted all string literals to global string constants"

        Bool b ->
            Output.code
                [ i <|
                    "ld a,"
                        ++ String.fromInt
                            (if b then
                                255

                             else
                                0
                            )
                ]

        BinOp data ->
            case data.op of
                BOp_Add ->
                    -- a = R
                    emitExpr ("right" :: "binop" :: ctx) data.right
                        |> Output.add (Output.code [ i "push af" ])
                        |> Output.add
                            -- a = L
                            (emitExpr ("left" :: "binop" :: ctx) data.left)
                        |> Output.add
                            (Output.code
                                [ i "pop bc" -- b = R
                                , i "add b" -- a = L + R
                                ]
                            )

                BOp_Sub ->
                    -- a = R
                    emitExpr ("right" :: "binop" :: ctx) data.right
                        |> Output.add (Output.code [ i "push af" ])
                        |> Output.add
                            -- a = L
                            (emitExpr ("left" :: "binop" :: ctx) data.left)
                        |> Output.add
                            (Output.code
                                [ i "pop bc" -- b = R
                                , i "sub b" -- a = L - R
                                ]
                            )

                BOp_Gt ->
                    let
                        ctxLabel =
                            Util.ctxLabel ctx

                        prefix =
                            "_gt_" ++ ctxLabel ++ "_"

                        endLabel =
                            prefix ++ "end"

                        onGTLabel =
                            prefix ++ "onGT"
                    in
                    -- a = L
                    emitExpr ("left" :: "binop" :: ctx) data.left
                        |> Output.add (Output.code [ i "push af" ])
                        |> Output.add
                            -- a = R
                            (emitExpr ("right" :: "binop" :: ctx) data.right)
                        |> Output.add
                            (Output.code
                                [ i "pop bc" -- b = L
                                , i "cp b" -- carry = L <= R
                                , i <| "jp nc," ++ onGTLabel

                                -- L <= R
                                , i "ld a,0"
                                , i <| "jp " ++ endLabel
                                , l onGTLabel -- L > R
                                , i "ld a,255"
                                , l endLabel
                                ]
                            )

                BOp_Lt ->
                    let
                        ctxLabel =
                            Util.ctxLabel ctx

                        prefix =
                            "_lt_" ++ ctxLabel ++ "_"

                        endLabel =
                            prefix ++ "end"

                        onLTLabel =
                            prefix ++ "onLT"
                    in
                    -- a = R
                    emitExpr ("right" :: "binop" :: ctx) data.right
                        |> Output.add (Output.code [ i "push af" ])
                        |> Output.add
                            -- a = L
                            (emitExpr ("left" :: "binop" :: ctx) data.left)
                        |> Output.add
                            (Output.code
                                [ i "pop bc" -- b = L
                                , i "cp b" -- carry = L >= R
                                , i <| "jp nc," ++ onLTLabel

                                -- L >= R
                                , i "ld a,0"
                                , i <| "jp " ++ endLabel
                                , l onLTLabel -- L < R
                                , i "ld a,255"
                                , l endLabel
                                ]
                            )

        UnaryOp data ->
            emitExpr ("unaryop" :: ctx) data.expr
                |> Output.add
                    (Output.code
                        [ i <|
                            case data.op of
                                UOp_Neg ->
                                    "neg"

                                UOp_Not ->
                                    -- We're using `neg` for boolean negation too. It flips between 0 and 255.
                                    "neg"
                        ]
                    )

        CallExpr data ->
            emitCall data

        IfExpr data ->
            emitIfExpr ctx data


{-| The result goes to the A register.
Otherwise this is pretty similar to the else-ful part of emitIfStmt
-}
emitIfExpr : List String -> IfExprData -> Output
emitIfExpr ctx data =
    let
        ctxLabel =
            Util.ctxLabel ctx

        labelPrefix =
            "_ifexpr_" ++ ctxLabel ++ "_"

        endLabel =
            labelPrefix ++ "end"

        elseLabel =
            labelPrefix ++ "else"
    in
    emitExpr ("cond" :: ctx) data.cond
        |> Output.add
            (Output.code
                [ i "cp 255"
                , i <| "jp nz," ++ elseLabel
                ]
            )
        |> Output.add (emitExpr ("then" :: ctx) data.then_)
        |> Output.add
            (Output.code
                [ i <| "jp " ++ endLabel
                , l elseLabel
                ]
            )
        |> Output.add (emitExpr ("else" :: ctx) data.else_)
        |> Output.add (Output.code [ l endLabel ])
