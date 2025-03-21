module F80.Emitter exposing (emit)

{-| Prepares assembly for consumption by the PASMO Z80 assembler.

Arguments to functions are passed on the stack.
Local variables are passed on the stack.
Function return values are in register A.
Expressions are emitted to the register A.

-}

import Dict exposing (Dict)
import F80.AST as AST
    exposing
        ( AssignData
        , BinOp(..)
        , Block
        , CallData
        , Decl(..)
        , DefineVarData
        , Expr(..)
        , FnDeclData
        , GlobalDeclData
        , IfExprData
        , IfStmtData
        , Param
        , Program
        , Stmt(..)
        , UnaryOp(..)
        , Value(..)
        , WaitForKeypressItem
        )
import F80.Emitter.Global
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.State as State exposing (State)
import F80.Emitter.Util as Util exposing (ctxLabel, i, l)
import F80.Emitter.WaitForKeypress
import List.Extra
import Set


emit : Program -> List String
emit program =
    List.Extra.indexedFoldl
        (\ix decl outputAndState ->
            case decl of
                GlobalDecl data ->
                    outputAndState
                        |> State.emit (F80.Emitter.Global.emit data)

                FnDecl data ->
                    outputAndState
                        |> State.withContext ("decl_" ++ String.fromInt ix) (emitFnDecl data)
        )
        (State.initWith State.empty)
        program
        |> Tuple.first
        |> Output.toString


{-| We assume the args will have been put on the stack during the function calls (emitCallArgs).
Any usage of the items on the stack will be emitted during emitExpr::Var.

Here we need to register the param names in `Vars`.

-}
emitFnDecl : FnDeclData -> State -> ( Output, State )
emitFnDecl fnData state =
    let
        isMain =
            fnData.name == AST.mainFnName

        blockFn : (State -> ( Output, State )) -> State -> ( Output, State )
        blockFn fn s =
            if isMain then
                fn s

            else
                State.initWith s
                    |> State.otherBlock fnData.name fn
    in
    State.initWith state
        |> State.withContext fnData.name
            (\stateFn ->
                State.initWith stateFn
                    |> State.withFrame fnData.name
                        (\stateFnFrame ->
                            State.initWith stateFnFrame
                                |> State.forEach fnData.params (\param -> State.lift_SS_SOS (State.addLocalVar param))
                                |> State.lift_SOS_OSOS
                                    (blockFn
                                        (\s ->
                                            State.initWith s
                                                |> State.l fnData.name
                                                |> State.emit (emitBlock { isMain = isMain } fnData.body)
                                        )
                                    )
                        )
            )


{-| These will emit their ASM blocks in the Output.mainCode field. That doesn't mean it's meant to go into `main()`!
-}
emitStmt : { isMain : Bool } -> Int -> Stmt -> State -> ( Output, State )
emitStmt isMain ix stmt state =
    State.initWith state
        |> State.withContext (String.fromInt ix)
            (\stateIx ->
                case stmt of
                    WaitForKeypress data ->
                        F80.Emitter.WaitForKeypress.emit (emitBlock isMain) data stateIx

                    Loop block ->
                        let
                            label =
                                Util.ctxLabel stateIx.ctx
                        in
                        State.initWith stateIx
                            |> State.withContext "loop"
                                (\stateIxLoop ->
                                    State.initWith stateIxLoop
                                        |> State.l label
                                        |> State.emit (emitBlock isMain block)
                                        |> State.i ("jp " ++ label)
                                )

                    If ifData ->
                        emitIfStmt isMain ifData stateIx

                    DefineConst defConst ->
                        emitDefineVar defConst stateIx

                    DefineLet defLet ->
                        emitDefineVar defLet stateIx

                    Assign assignData ->
                        emitAssign assignData

                    CallStmt callData ->
                        emitCall stateIx callData

                    Return maybeExpr ->
                        emitReturn isMain stateIx maybeExpr
            )


emitDefineVar : DefineVarData -> State -> ( Output, State )
emitDefineVar defVar state =
    State.initWith state
        |> State.emit (emitExpr defVar.value)
        |> State.i "push af"
        |> State.lift_SS_OSOS (State.addLocalVar defVar.name)


emitReturn : { isMain : Bool } -> State -> Maybe Expr -> ( Output, State )
emitReturn { isMain } state maybeExpr =
    let
        return : ( Output, State ) -> ( Output, State )
        return =
            if isMain then
                State.i "jp _end"

            else
                State.i "ret"
    in
    case maybeExpr of
        Nothing ->
            State.initWith state
                |> return

        Just expr ->
            State.initWith state
                |> State.emit (emitExpr expr)
                |> return


emitIfStmt : { isMain : Bool } -> IfStmtData -> State -> ( Output, State )
emitIfStmt isMain ifData state =
    let
        ctxLabel =
            Util.ctxLabel state.ctx

        labelPrefix =
            "_ifstmt_" ++ ctxLabel ++ "_"

        endLabel =
            labelPrefix ++ "end"
    in
    case ifData.else_ of
        Nothing ->
            State.initWith state
                |> State.withContext "cond" (emitExpr ifData.cond)
                |> State.i "cp 255"
                |> State.i ("jp nz," ++ endLabel)
                -- then:
                |> State.withContext "then" (emitBlock isMain ifData.then_)
                -- end:
                |> State.l endLabel

        Just else_ ->
            let
                elseLabel =
                    labelPrefix ++ "else"
            in
            State.initWith state
                |> State.withContext "cond" (emitExpr ifData.cond)
                |> State.i "cp 255"
                |> State.i ("jp nz," ++ elseLabel)
                -- then:
                |> State.withContext "then" (emitBlock isMain ifData.then_)
                |> State.i ("jp " ++ endLabel)
                -- else:
                |> State.l elseLabel
                |> State.withContext "else" (emitBlock isMain else_)
                -- end:
                |> State.l endLabel


emitBlock : { isMain : Bool } -> Block -> State -> ( Output, State )
emitBlock isMain block state =
    List.Extra.indexedFoldl
        (\ix stmt outputAndState ->
            outputAndState
                |> State.emit (emitStmt isMain ix stmt)
        )
        (State.initWith state)
        block


emitAssign : AssignData -> ( Output, State )
emitAssign assignData =
    Debug.todo "emitAssign"


emitCall : State -> CallData -> ( Output, State )
emitCall state callData =
    case callData.fn of
        "ROM.clearScreen" ->
            ( Output.romCls, state )

        "Render.text" ->
            ( emitCallRenderText callData, state )

        _ ->
            let
                ( argsOutput, newState ) =
                    emitCallArgs state callData.args
            in
            ( argsOutput
                |> Output.add
                    (Output.code
                        [ i <| "call " ++ callData.fn ]
                    )
            , newState
            )


emitCallArgs : State -> List Expr -> ( Output, State )
emitCallArgs state args =
    List.Extra.indexedFoldl
        (\ix arg outputAndState ->
            outputAndState
                |> State.emit (emitExpr arg)
                |> State.i "push af"
        )
        (State.initWith state)
        args


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


{-| Loads the value into the register A.
-}
emitExpr : Expr -> State -> ( Output, State )
emitExpr expr state =
    let
        ( output, finalState ) =
            case expr of
                Int n ->
                    ( Output.code [ i <| "ld a," ++ String.fromInt n ]
                    , state
                    )

                Var var ->
                    ( if Set.member var state.globalVars then
                        Output.code [ i <| "ld a," ++ var ]

                      else
                        case State.getVar var state of
                            Nothing ->
                                Debug.todo <| "emitExpr: var " ++ var ++ " not found"

                            Just { stackOffset } ->
                                Output.code
                                    [ i <| "ld ix," ++ String.fromInt (State.getCurrentFrameSize state)
                                    , i "add ix,sp"
                                    , i <| "ld a,(ix-" ++ String.fromInt stackOffset ++ ")"
                                    ]
                    , state
                    )

                String _ ->
                    Debug.todo "emitExpr String - this shouldn't have happened - we hoisted all string literals to global string constants"

                Bool b ->
                    ( Output.code
                        [ i <|
                            "ld a,"
                                ++ String.fromInt
                                    (if b then
                                        255

                                     else
                                        0
                                    )
                        ]
                    , state
                    )

                BinOp data ->
                    let
                        fn : State -> ( Output, State )
                        fn stateBinop =
                            case data.op of
                                BOp_Add ->
                                    State.initWith stateBinop
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> State.i "push af"
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> {- b = R -} State.i "pop bc"
                                        |> {- a = L + R -} State.i "add b"

                                BOp_Sub ->
                                    State.initWith stateBinop
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> State.i "push af"
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> {- b = R -} State.i "pop bc"
                                        |> {- a = L - R -} State.i "sub b"

                                BOp_Gt ->
                                    let
                                        ctxLabel =
                                            Util.ctxLabel stateBinop.ctx

                                        prefix =
                                            "_gt_" ++ ctxLabel ++ "_"

                                        endLabel =
                                            prefix ++ "end"

                                        onGTLabel =
                                            prefix ++ "onGT"
                                    in
                                    State.initWith stateBinop
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> State.i "push af"
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> {- b = L -} State.i "pop bc"
                                        |> {- carry = L <= R -} State.i "cp b"
                                        |> State.i ("jp nc," ++ onGTLabel)
                                        -- L <= R:
                                        |> State.i "ld a,0"
                                        |> State.i ("jp " ++ endLabel)
                                        -- L > R:
                                        |> State.l onGTLabel
                                        |> State.i "ld a,255"
                                        |> State.l endLabel

                                BOp_Lt ->
                                    let
                                        ctxLabel =
                                            Util.ctxLabel stateBinop.ctx

                                        prefix =
                                            "_lt_" ++ ctxLabel ++ "_"

                                        endLabel =
                                            prefix ++ "end"

                                        onLTLabel =
                                            prefix ++ "onLT"
                                    in
                                    State.initWith stateBinop
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> State.i "push af"
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> {- b = R -} State.i "pop bc"
                                        |> {- carry = L >= R -} State.i "cp b"
                                        |> State.i ("jp nc," ++ onLTLabel)
                                        -- L >= R:
                                        |> State.i "ld a,0"
                                        |> State.i ("jp " ++ endLabel)
                                        -- L < R:
                                        |> State.l onLTLabel
                                        |> State.i "ld a,255"
                                        |> State.l endLabel
                    in
                    State.initWith state
                        |> State.withContext "binop" fn

                UnaryOp data ->
                    let
                        fn : State -> ( Output, State )
                        fn stateUnaryop =
                            case data.op of
                                UOp_Neg ->
                                    State.initWith stateUnaryop
                                        |> State.emit (emitExpr data.expr)
                                        |> State.i "neg"

                                UOp_Not ->
                                    State.initWith stateUnaryop
                                        |> State.emit (emitExpr data.expr)
                                        |> State.i "neg"
                    in
                    State.initWith state
                        |> State.withContext "unaryop" fn

                CallExpr data ->
                    emitCall state data

                IfExpr data ->
                    emitIfExpr state data
    in
    ( output, finalState )


{-| The result goes to the A register.
Otherwise this is pretty similar to the else-ful part of emitIfStmt
-}
emitIfExpr : State -> IfExprData -> ( Output, State )
emitIfExpr state data =
    let
        ctxLabel =
            Util.ctxLabel state.ctx

        labelPrefix =
            "_ifexpr_" ++ ctxLabel ++ "_"

        endLabel =
            labelPrefix ++ "end"

        elseLabel =
            labelPrefix ++ "else"
    in
    State.initWith state
        |> State.withContext "if"
            (\stateIf ->
                State.initWith stateIf
                    |> State.withContext "cond" (emitExpr data.cond)
                    |> State.i "cp 255"
                    |> State.i ("jp nz," ++ elseLabel)
                    |> State.withContext "then" (emitExpr data.then_)
                    |> State.i ("jp " ++ endLabel)
                    |> State.l elseLabel
                    |> State.withContext "else" (emitExpr data.else_)
                    |> State.l endLabel
            )
