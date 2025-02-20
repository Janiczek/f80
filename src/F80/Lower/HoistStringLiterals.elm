module F80.Lower.HoistStringLiterals exposing (hoist)

{-| The purpose of this lowering is to convert string literals in function calls
etc. to vars to globally defined strings, so that Render.text() can work with them uniformly. (The emitter generates a label for each global string constant and )
-}

import Dict exposing (Dict)
import F80.AST exposing (Decl(..), Expr(..), Program, Stmt(..), Value(..))


var : Int -> Int -> String
var ix jx =
    "_string_" ++ String.fromInt ix ++ "_" ++ String.fromInt jx


hoist : Program -> Program
hoist program =
    program
        |> List.indexedMap
            (\ix decl ->
                case decl of
                    GlobalDecl decl_ ->
                        [ decl ]

                    FnDecl decl_ ->
                        let
                            ( ( _, jxBody, stringsBody ), newBody ) =
                                F80.AST.walkBlock
                                    rememberString
                                    (\acc stmt -> ( acc, stmt ))
                                    ( ix, 0, Dict.empty )
                                    decl_.body
                        in
                        List.concat
                            [ stringsBody
                                |> Dict.toList
                                |> List.map
                                    (\( ( ix_, jx_ ), string ) ->
                                        GlobalDecl
                                            { name = var ix_ jx_
                                            , value = VString string
                                            }
                                    )
                            , [ FnDecl { decl_ | body = newBody } ]
                            ]
            )
        |> List.concat


rememberString :
    ( Int, Int, Dict ( Int, Int ) String )
    -> Expr
    -> ( ( Int, Int, Dict ( Int, Int ) String ), Expr )
rememberString ( ix, jx, strings ) expr =
    case expr of
        String str ->
            ( ( ix
              , jx + 1
              , Dict.insert ( ix, jx ) str strings
              )
            , Var (var ix jx)
            )

        _ ->
            ( ( ix, jx, strings )
            , expr
            )
