module F80.Emitter exposing (emit)

{-| Prepares assembly for consumption by the PASMO Z80 assembler.
-}

import Dict exposing (Dict)
import F80.AST
    exposing
        ( AssignData
        , BinOp(..)
        , Block
        , CallData
        , CallRenderTextStmtData
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
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.Util as Util exposing (ctxLabel, i, l)
import Hex


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
            F80.Emitter.Global.emitGlobalDecl data

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
            fnData.name == Util.mainFnName

        blockOutput =
            emitBlock (fnData.name :: ctx) fnData.body

        blockWithoutCode =
            { blockOutput | mainCode = [] }
    in
    if isMain then
        Output.code
            (List.concat
                [ [ l fnData.name ]
                , blockOutput.mainCode
                ]
            )
            |> Output.add blockWithoutCode

    else
        Output.fn
            (List.concat
                [ [ l fnData.name ]
                , blockOutput.mainCode
                , [ i "ret" ]
                ]
            )
            |> Output.add blockWithoutCode


{-| These will emit their ASM blocks in the Output.mainCode field. That doesn't mean it's meant to go into `main()`!
-}
emitStmt : List String -> Int -> Stmt -> Output
emitStmt parentCtx ix stmt =
    let
        ctx =
            String.fromInt ix :: parentCtx
    in
    case stmt of
        WaitForKeyboard _ ->
            Debug.todo "emitStmt WaitForKeyboard"

        Loop block ->
            let
                label =
                    Util.ctxLabel ctx
            in
            Output.code [ l label ]
                |> Output.add (emitBlock ("loop" :: ctx) block)
                |> Output.add (Output.code [ i <| "jp " ++ label ])

        If ifData ->
            emitIf ifData

        DefineConst defConst ->
            Debug.todo <| "emitStmt defineConst: " ++ Debug.toString defConst

        DefineLet defLet ->
            Debug.todo <| "emitStmt defineLet: " ++ Debug.toString defLet

        Assign assignData ->
            emitAssign assignData

        CallStmt callData ->
            emitCall callData

        CallRenderTextStmt callData ->
            emitCallRenderText callData


emitIf : IfStmtData -> Output
emitIf ifData =
    Debug.todo "emitIf"


emitBlock : List String -> Block -> Output
emitBlock ctx block =
    block
        |> List.indexedMap (emitStmt ctx)
        |> List.foldl Output.add Output.empty


emitAssign : AssignData -> Output
emitAssign assignData =
    Debug.todo "emitAssign"


emitCall : CallData -> Output
emitCall callData =
    Output.code
        (List.concat
            [ pushArgs callData.args
            , [ i <| "call " ++ callData.fn ]
            ]
        )


emitCallRenderText : CallRenderTextStmtData -> Output
emitCallRenderText callData =
    Output.code
        (List.concat
            [ renderTextXYHL callData.x callData.y
            , [ case callData.string of
                    Var name ->
                        i <| "ld de," ++ name

                    String str ->
                        Debug.todo "Render.text - this should have been caught - we should have hoisted the string literal to a global constant and changed this call to use the var"

                    _ ->
                        let
                            _ =
                                Debug.log "Unexpected Render.text string argument" callData.string
                        in
                        Debug.todo "Unexpected Render.text string argument - this should have been typechecked before emitting"
              , i <| "call renderString"
              ]
            ]
        )
        |> Output.add Output.renderText


renderTextXYHL : Expr -> Expr -> List String
renderTextXYHL x y =
    let
        emitInt : String -> Int -> String
        emitInt reg n =
            Hex.toString n
                |> String.padLeft 2 '0'
                |> add0x
                |> (\hex -> i <| "ld " ++ reg ++ "," ++ hex)

        emitVar : String -> String -> String
        emitVar reg var =
            Debug.todo "TODO emit var"
    in
    case ( x, y ) of
        ( Int xx, Int yy ) ->
            -- Special optimized case
            -- ld hl,0xXXYY
            (xx * 256 + yy)
                |> Hex.toString
                |> String.padLeft 4 '0'
                |> add0x
                |> (\hex -> [ i <| "ld hl," ++ hex ])

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


add0x : String -> String
add0x s =
    "0x" ++ s


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
