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
import NonemptyList
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
                    |> State.withFrame
                        { name = fnData.name
                        , params = fnData.params
                        , type_ =
                            if isMain then
                                State.MainFn

                            else
                                State.NonMainFn
                        }
                        (\stateFnFrame ->
                            State.initWith stateFnFrame
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


type alias FnInfo =
    { isMain : Bool
    }


{-| These will emit their ASM blocks in the Output.mainCode field. That doesn't mean it's meant to go into `main()`!
-}
emitStmt : FnInfo -> Int -> Stmt -> State -> ( Output, State )
emitStmt fnInfo ix stmt state =
    State.initWith state
        |> State.withContext (String.fromInt ix)
            (\stateIx ->
                case stmt of
                    WaitForKeypress data ->
                        F80.Emitter.WaitForKeypress.emit (emitBlock fnInfo) data stateIx

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
                                        |> State.emit (emitBlock fnInfo block)
                                        |> State.i ("jp " ++ label)
                                )

                    If ifData ->
                        emitIfStmt fnInfo ifData stateIx

                    DefineConst defConst ->
                        emitDefineVar defConst stateIx

                    DefineLet defLet ->
                        emitDefineVar defLet stateIx

                    Assign assignData ->
                        emitAssign assignData stateIx

                    CallStmt callData ->
                        emitCall stateIx callData

                    Return maybeExpr ->
                        emitReturn fnInfo stateIx maybeExpr
            )


emitDefineVar : DefineVarData -> State -> ( Output, State )
emitDefineVar defVar state =
    State.initWith state
        |> State.emit (emitExpr defVar.value)
        |> State.push "af" { countAsExtra = False }
        |> State.lift_SS_OSOS (State.addLocalVar { isArg = False } defVar.name)


emitReturn : { isMain : Bool } -> State -> Maybe Expr -> ( Output, State )
emitReturn { isMain } state maybeExpr =
    let
        return : ( Output, State ) -> ( Output, State )
        return =
            if isMain then
                State.i "jp _end"

            else
                State.i "ret"

        cleanup =
            if isMain then
                identity

            else
                State.cleanupStack (State.countCurrentLocals state)
    in
    case maybeExpr of
        Nothing ->
            State.initWith state
                |> cleanup
                |> return

        Just expr ->
            State.initWith state
                |> State.emit (emitExpr expr)
                |> cleanup
                |> return


emitIfStmt : FnInfo -> IfStmtData -> State -> ( Output, State )
emitIfStmt fnInfo ifData state =
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
                |> State.i ("cp " ++ emitBool True)
                |> State.i ("jp nz," ++ endLabel)
                -- then:
                |> State.withContext "then" (emitBlock fnInfo ifData.then_)
                -- end:
                |> State.l endLabel

        Just else_ ->
            let
                elseLabel =
                    labelPrefix ++ "else"
            in
            State.initWith state
                |> State.withContext "cond" (emitExpr ifData.cond)
                |> State.i ("cp " ++ emitBool True)
                |> State.i ("jp nz," ++ elseLabel)
                -- then:
                |> State.withContext "then" (emitBlock fnInfo ifData.then_)
                |> State.i ("jp " ++ endLabel)
                -- else:
                |> State.l elseLabel
                |> State.withContext "else" (emitBlock fnInfo else_)
                -- end:
                |> State.l endLabel


emitBlock : FnInfo -> Block -> State -> ( Output, State )
emitBlock fnInfo block state =
    List.Extra.indexedFoldl
        (\ix stmt outputAndState ->
            outputAndState
                |> State.emit (emitStmt fnInfo ix stmt)
        )
        (State.initWith state)
        block


emitAssign : AssignData -> State -> ( Output, State )
emitAssign assignData state =
    case State.getVar assignData.var state of
        Nothing ->
            Debug.todo <| "emitAssign: var '" ++ assignData.var ++ "' not found"

        Just { stackOffset } ->
            case assignData.op of
                Nothing ->
                    -- case without an op
                    State.initWith state
                        |> State.emit (emitExpr assignData.value)
                        |> State.i ("ld ix," ++ String.fromInt (State.currentBaseOffset state))
                        |> State.i "add ix,sp"
                        |> State.i ("ld (ix-" ++ String.fromInt stackOffset ++ "),a")

                Just op ->
                    Debug.todo <| "emitAssign: TODO case with op " ++ Debug.toString op


emitCall : State -> CallData -> ( Output, State )
emitCall state callData =
    case callData.fn of
        "ROM.clearScreen" ->
            ( Output.romCls, state )

        "Render.text" ->
            ( emitCallRenderText callData, state )

        _ ->
            State.initWith state
                |> State.emit (emitCallArgs callData.args)
                |> State.i ("call " ++ callData.fn)
                |> State.cleanupStack (List.length callData.args)


emitCallArgs : List Expr -> State -> ( Output, State )
emitCallArgs args state =
    State.initWith state
        |> State.forEach args
            (\arg s ->
                State.initWith s
                    |> State.emit (emitExpr arg)
                    |> State.push "af" { countAsExtra = True }
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


{-| Loads the value into the register A.
-}
emitExpr : Expr -> State -> ( Output, State )
emitExpr expr state =
    let
        ( output, finalState ) =
            case expr of
                Int n ->
                    State.initWith state
                        |> State.i ("ld a," ++ String.fromInt n)

                Var var ->
                    State.initWith state
                        |> (if Set.member var state.globalVars then
                                State.i ("ld a," ++ var)

                            else
                                case State.getVar var state of
                                    Nothing ->
                                        Debug.todo <| "emitExpr: var " ++ var ++ " not found"

                                    Just { stackOffset } ->
                                        \os ->
                                            os
                                                |> State.i ("ld ix," ++ String.fromInt (State.currentBaseOffset state))
                                                |> State.i "add ix,sp"
                                                |> State.i ("ld a,(ix-" ++ String.fromInt stackOffset ++ ")")
                           )

                String _ ->
                    Debug.todo "emitExpr String - this shouldn't have happened - we hoisted all string literals to global string constants"

                Bool b ->
                    State.initWith state
                        |> State.i
                            ("ld a," ++ emitBool b)

                BinOp data ->
                    let
                        fn : State -> ( Output, State )
                        fn stateBinop =
                            case data.op of
                                BOp_Add ->
                                    State.initWith stateBinop
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> State.push "af" { countAsExtra = True }
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> {- b = R -} State.pop "bc" { countAsExtra = True }
                                        |> {- a = L + R -} State.i "add b"

                                BOp_Sub ->
                                    State.initWith stateBinop
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> State.push "af" { countAsExtra = True }
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> {- b = R -} State.pop "bc" { countAsExtra = True }
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
                                        |> State.push "af" { countAsExtra = True }
                                        |> {- a = R -} State.withContext "right" (emitExpr data.right)
                                        |> {- b = L -} State.pop "bc" { countAsExtra = True }
                                        |> {- carry = L <= R -} State.i "cp b"
                                        |> State.i ("jp nc," ++ onGTLabel)
                                        -- L <= R:
                                        |> State.i ("ld a," ++ emitBool False)
                                        |> State.i ("jp " ++ endLabel)
                                        -- L > R:
                                        |> State.l onGTLabel
                                        |> State.i ("ld a," ++ emitBool True)
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
                                        |> State.push "af" { countAsExtra = True }
                                        |> {- a = L -} State.withContext "left" (emitExpr data.left)
                                        |> {- b = R -} State.pop "bc" { countAsExtra = True }
                                        |> {- carry = L >= R -} State.i "cp b"
                                        |> State.i ("jp nc," ++ onLTLabel)
                                        -- L >= R:
                                        |> State.i ("ld a," ++ emitBool False)
                                        |> State.i ("jp " ++ endLabel)
                                        -- L < R:
                                        |> State.l onLTLabel
                                        |> State.i ("ld a," ++ emitBool True)
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
                    |> State.i ("cp " ++ emitBool True)
                    |> State.i ("jp nz," ++ elseLabel)
                    |> State.withContext "then" (emitExpr data.then_)
                    |> State.i ("jp " ++ endLabel)
                    |> State.l elseLabel
                    |> State.withContext "else" (emitExpr data.else_)
                    |> State.l endLabel
            )


emitBool : Bool -> String
emitBool b =
    String.fromInt
        (if b then
            255

         else
            0
        )
