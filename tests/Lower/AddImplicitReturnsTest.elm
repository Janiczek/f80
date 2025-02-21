module Lower.AddImplicitReturnsTest exposing (..)

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
import F80.AST.ToString
import F80.Lower.AddImplicitReturns
import F80.Parser
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


testLower : String -> String -> Test
testLower source expected =
    Test.test ("Lowering: " ++ source) <|
        \() ->
            case F80.Parser.parse source of
                Ok decls ->
                    decls
                        |> F80.Lower.AddImplicitReturns.add
                        |> F80.AST.ToString.toString
                        |> Expect.equal (String.trim expected)

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.describe "F80.Lower.AddImplicitReturns.add"
        [ mainFn
        , nonMainFn
        ]


mainFn : Test
mainFn =
    Test.describe "mainFn"
        [ testLower
            """
main() {
}
            """
            """
main() {
}
            """
        , testLower
            """
main() {
    ROM.clearScreen()
}
            """
            """
main() {
    ROM.clearScreen()
}
            """
        , testLower
            """
main() {
    return
}
            """
            """
main() {
    return
}
            """
        , testLower
            """
main() {
    return 1
}
            """
            """
main() {
    return 1
}
            """
        , testLower
            """
main() {
    if (true) {
        return
    }
    ROM.clearScreen()
}
            """
            """
main() {
    if (true) {
        return
    }
    ROM.clearScreen()
}
            """
        ]


nonMainFn : Test
nonMainFn =
    Test.describe "nonMainFn"
        [ testLower
            """
main() {
}
fn() {
}
            """
            """
main() {
}
fn() {
    return
}
            """
        , testLower
            """
main() {
    fn()
}
fn() {
    ROM.clearScreen()
}
            """
            """
main() {
    fn()
}
fn() {
    ROM.clearScreen()
    return
}
            """
        , testLower
            """
main() {
    return fn()
}
fn() {
    return 1
}
            """
            """
main() {
    return fn()
}
fn() {
    return 1
}
            """
        , testLower
            """
main() {
    return fn()
}
fn() {
    return
}
            """
            """
main() {
    return fn()
}
fn() {
    return
}
            """
        , testLower
            """
main() {
    fn()
}
fn() {
    if (true) {
        return
    }
    ROM.clearScreen()
}
            """
            """
main() {
    fn()
}
fn() {
    if (true) {
        return
    }
    ROM.clearScreen()
    return
}
            """
        ]
