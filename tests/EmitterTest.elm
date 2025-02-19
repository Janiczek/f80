module EmitterTest exposing (suite)

import Example
import Expect exposing (Expectation)
import F80.AST
    exposing
        ( BinOp(..)
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Stmt(..)
        , Value(..)
        )
import F80.Emitter
import F80.Parser
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


{-| Converts the given source code to a list of Decl, emits it, and compares it
to the expected single string. It splits the expected string by newlines
for comparison against the list of lines from F80.Emitter.emit.
-}
testEmit : String -> String -> Test
testEmit source expected =
    Test.test ("Emitting: " ++ source) <|
        \() ->
            case F80.Parser.parse source of
                Ok decls ->
                    decls
                        |> F80.Emitter.emit
                        |> Expect.equal (expected |> String.split "\n")

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.describe "F80.Emitter.emit"
        [ globals
        ]


globals : Test
globals =
    Test.describe "globals"
        [ testEmit
            "const x = 1"
            "x db 1"
        , testEmit
            "const str = \"Hello World!\""
            """str db 'Hello World!', 0
str_length EQU 12"""
        , testEmit
            "const bytes = [1,2,3]"
            "bytes db 1, 2, 3"
        , testEmit
            "const other = global"
            "other EQU global"
        , testEmit
            "const binop = 1 + 2"
            "binop db 1 + 2"
        , testEmit
            """const len = String.length("Hello World!")"""
            "len db 12"
        , testEmit
            "const len = String.length(hello)"
            "len db hello_length"
        ]
