module F80.Emitter exposing (emit)

{-| Prepares assembly for consumption by the PASMO Z80 assembler.

Arguments to functions are passed on the stack.
Local variables are passed on the stack.

Function return values and expressions are emitted to the register A, unless
they're a pointer (like to a string) in which case they don't fit into a single
8bit register and they'll be in HL.

TODO: make everything be in H or HL? Or make things be on the stack?

-}

import Dict exposing (Dict)
import F80.AST as AST
    exposing
        ( AssignData
        , BinOp(..)
        , Block
        , CallData
        , Decl(..)
        , DefineConstData
        , DefineLetData
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
import F80.AST.ToString
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
            AST.isMain fnData.name

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
                                                |> State.emit (emitBlockWithoutFrame { isMain = isMain } fnData.body)
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
                                        |> State.emit (emitBlock fnInfo "loop" block)
                                        |> State.i ("jp " ++ label)
                                )

                    If ifData ->
                        emitIfStmt fnInfo ifData stateIx

                    DefineConst defConst ->
                        emitDefineConst defConst stateIx

                    DefineLet defLet ->
                        emitDefineLet defLet stateIx

                    Assign assignData ->
                        emitAssign assignData stateIx

                    CallStmt callData ->
                        emitCall callData stateIx

                    Return maybeExpr ->
                        emitReturn fnInfo maybeExpr stateIx
            )


emitDefineConst : DefineConstData -> State -> ( Output, State )
emitDefineConst defVar state =
    State.initWith state
        |> State.emit (emitExpr defVar.value)
        |> State.push "af" { countAsExtra = False }
        |> State.lift_SS_OSOS (State.addLocalVar { type_ = State.Const } defVar.name)


emitDefineLet : DefineLetData -> State -> ( Output, State )
emitDefineLet defVar state =
    State.initWith state
        |> State.emit (emitExpr defVar.value)
        |> State.push "af" { countAsExtra = False }
        |> State.lift_SS_OSOS (State.addLocalVar { type_ = State.Let } defVar.name)


emitReturn : { isMain : Bool } -> Maybe Expr -> State -> ( Output, State )
emitReturn { isMain } maybeExpr state =
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
                |> State.withContext "then" (emitBlock fnInfo "$then" ifData.then_)
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
                |> State.withContext "then" (emitBlock fnInfo "$then" ifData.then_)
                |> State.i ("jp " ++ endLabel)
                -- else:
                |> State.l elseLabel
                |> State.withContext "else" (emitBlock fnInfo "$else" else_)
                -- end:
                |> State.l endLabel


emitBlock : FnInfo -> String -> Block -> State -> ( Output, State )
emitBlock fnInfo blockId block state =
    State.initWith state
        |> State.withFrame
            { name = blockId
            , params = []
            , type_ = State.Block
            }
            (\stateFrame ->
                let
                    sizeBefore =
                        State.countCurrentLocals stateFrame

                    ( o, finalState ) =
                        stateFrame
                            |> emitBlockWithoutFrame fnInfo block

                    sizeAfter =
                        State.countCurrentLocals finalState

                    diff =
                        sizeAfter - sizeBefore
                in
                ( o, finalState )
                    |> State.cleanupStack diff
            )


emitBlockWithoutFrame : FnInfo -> Block -> State -> ( Output, State )
emitBlockWithoutFrame fnInfo block state =
    State.initWith state
        |> State.forEachIndexed block (emitStmt fnInfo)


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
                    case op of
                        BOp_Add ->
                            State.initWith state
                                |> State.emit (emitExpr assignData.value)
                                |> State.i "ld b,a"
                                |> State.i ("ld ix," ++ String.fromInt (State.currentBaseOffset state))
                                |> State.i "add ix,sp"
                                |> State.i ("ld a,(ix-" ++ String.fromInt stackOffset ++ ")")
                                |> State.i "add b"
                                -- we don't need to prepare ix again, it's still good from above!
                                |> State.i ("ld (ix-" ++ String.fromInt stackOffset ++ "),a")

                        BOp_Sub ->
                            State.initWith state
                                |> State.emit (emitExpr assignData.value)
                                |> State.i "ld b,a"
                                |> State.i ("ld ix," ++ String.fromInt (State.currentBaseOffset state))
                                |> State.i "add ix,sp"
                                |> State.i ("ld a,(ix-" ++ String.fromInt stackOffset ++ ")")
                                |> State.i "sub b"
                                -- we don't need to prepare ix again, it's still good from above!
                                |> State.i ("ld (ix-" ++ String.fromInt stackOffset ++ "),a")

                        BOp_Lt ->
                            Debug.todo <| "emitAssign: assignment with op Lt shouldn't be possible. Parser bug?"

                        BOp_Gt ->
                            Debug.todo <| "emitAssign: assignment with op Gt shouldn't be possible. Parser bug?"


emitCall : CallData -> State -> ( Output, State )
emitCall callData state =
    case callData.fn of
        "ROM.clearScreen" ->
            ( Output.romCls, state )

        "Render.text" ->
            emitCallRenderText callData state

        "String.fromU8" ->
            emitCallStringFromU8 callData state

        "U8.divMod" ->
            ( emitCallU8DivMod callData, state )

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
emitCallRenderText : CallData -> State -> ( Output, State )
emitCallRenderText callData state =
    case callData.args of
        [ x, y, string ] ->
            state
                |> renderTextXYHL x y
                |> State.emit
                    (case string of
                        Var name ->
                            emitExpr (Var name)

                        String str ->
                            Debug.todo "Render.text - this should have been caught - we should have hoisted the string literal to a global constant and changed this call to use the var"

                        _ ->
                            let
                                _ =
                                    Debug.log "Unexpected Render.text string argument" string
                            in
                            Debug.todo "Unexpected Render.text string argument - this should have been typechecked before emitting"
                    )
                |> State.i "call _renderText"
                |> State.addOutput Output.renderText

        _ ->
            Debug.todo "emitCallRenderText - unexpected number of arguments"


renderTextXYHL : Expr -> Expr -> State -> ( Output, State )
renderTextXYHL x y state =
    let
        emitInt : String -> Int -> ( Output, State ) -> ( Output, State )
        emitInt reg n =
            State.i ("ld " ++ reg ++ "," ++ String.fromInt n)

        emitVar : String -> String -> ( Output, State ) -> ( Output, State )
        emitVar reg var =
            State.emit (emitExpr (Var var))
                >> State.i ("ld " ++ reg ++ ",a")
    in
    case ( x, y ) of
        ( Int xx, Int yy ) ->
            -- Special optimized case
            -- ld hl,0xXXYY
            State.initWith state
                |> State.i ("ld hl," ++ String.fromInt (xx * 256 + yy))

        ( Int xx, Var yy ) ->
            State.initWith state
                |> emitInt "h" xx
                |> emitVar "l" yy

        ( Var xx, Int yy ) ->
            State.initWith state
                |> emitVar "h" xx
                |> emitInt "l" yy

        ( Var xx, Var yy ) ->
            State.initWith state
                |> emitVar "h" xx
                |> emitVar "l" yy

        ( _, _ ) ->
            let
                _ =
                    Debug.log "Unexpected Render.text X,Y argument" ( x, y )
            in
            Debug.todo "Unexpected Render.text X,Y argument - this should have been typechecked before emitting"


emitCallStringFromU8 : CallData -> State -> ( Output, State )
emitCallStringFromU8 callData state =
    case callData.args of
        [ n ] ->
            State.initWith state
                |> State.emit (emitExpr n)
                |> State.i "call _stringFromU8"
                |> State.addOutput Output.stringFromU8

        _ ->
            Debug.todo "emitCallStringFromU8 - unexpected number of arguments"


emitCallU8DivMod : CallData -> Output
emitCallU8DivMod callData =
    case callData.args of
        [ n, d ] ->
            case ( n, d ) of
                ( Int n_, Int d_ ) ->
                    Output.code
                        [ i <| "ld a," ++ String.fromInt n_
                        , i <| "ld b," ++ String.fromInt d_
                        , i <| "call _u8DivMod"

                        -- TODO do something with the results?
                        ]
                        |> Output.add Output.u8DivMod

                _ ->
                    Debug.todo <|
                        "emitCallU8DivMod: unexpected arguments: "
                            ++ F80.AST.ToString.exprToString n
                            ++ ", "
                            ++ F80.AST.ToString.exprToString d

        _ ->
            Debug.todo "emitCallU8DivMod - unexpected number of arguments"


{-| Loads the value into the register A, unless it's a String var, which goes to HL.
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
                                if Debug.todo "is global var a string" then
                                    State.i ("ld hl," ++ var)

                                else
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
                                                |> (if Debug.todo "is local var a string" then
                                                        \os2 ->
                                                            os2
                                                                -- TODO recheck this
                                                                |> State.i ("ld h,(ix-" ++ String.fromInt stackOffset ++ ")")
                                                                |> State.i ("ld l,(ix-" ++ String.fromInt (stackOffset + 1) ++ ")")

                                                    else
                                                        State.i ("ld a,(ix-" ++ String.fromInt stackOffset ++ ")")
                                                   )
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
                                UOp_Not ->
                                    State.initWith stateUnaryop
                                        |> State.emit (emitExpr data.expr)
                                        -- 0 -> 255, 255 -> 0
                                        |> State.i "neg"
                    in
                    State.initWith state
                        |> State.withContext "unaryop" fn

                CallExpr data ->
                    emitCall data state

                IfExpr data ->
                    emitIfExpr data state
    in
    ( output, finalState )


{-| The result goes to the A register.
Otherwise this is pretty similar to the else-ful part of emitIfStmt
-}
emitIfExpr : IfExprData -> State -> ( Output, State )
emitIfExpr data state =
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
