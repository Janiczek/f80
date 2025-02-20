module Lower.HoistStringLiteralsTest exposing (suite)

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
import F80.Lower.HoistStringLiterals
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
                        |> F80.Lower.HoistStringLiterals.hoist
                        |> F80.AST.ToString.toString
                        |> Expect.equal (String.trim expected)

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.describe "F80.Lower.HoistStringLiterals.hoist"
        [ stmts
        ]


stmts : Test
stmts =
    Test.describe "stmts"
        [ testLower
            """
main() {
    const x = "hello"
}
            """
            """
const _string_0_0 = "hello"
main() {
    const x = _string_0_0
}
            """
        , testLower
            """
main() {
    const x = "hello"
    const y = "world"
}
            """
            """
const _string_0_0 = "hello"
const _string_0_1 = "world"
main() {
    const x = _string_0_0
    const y = _string_0_1
}
            """
        , testLower
            """
main() {
    let x = "hello"
}
            """
            """
const _string_0_0 = "hello"
main() {
    let x = _string_0_0
}
            """
        , testLower
            """
main() {
    let x = 1
    x = "abc"
}
            """
            """
const _string_0_0 = "abc"
main() {
    let x = 1
    x = _string_0_0
}
            """
        , testLower
            """
main() {
    foo("abc")
}
            """
            """
const _string_0_0 = "abc"
main() {
    foo(_string_0_0)
}
            """
        , testLower
            """
main() {
    foo("abc","def")
}
            """
            """
const _string_0_0 = "abc"
const _string_0_1 = "def"
main() {
    foo(_string_0_0, _string_0_1)
}
            """
        , testLower
            """
main() {
    foo(1,"def",3)
}
            """
            """
const _string_0_0 = "def"
main() {
    foo(1, _string_0_0, 3)
}
            """
        , testLower
            """
main() {
    Render.text(0,1,"def")
}
            """
            """
const _string_0_0 = "def"
main() {
    Render.text(0, 1, _string_0_0)
}
            """
        , testLower
            """
main() {
    let x = 1
    if ("a") {
        x = 1
    } else {
        x = 2
    }
}
            """
            """
const _string_0_0 = "a"
main() {
    let x = 1
    if (_string_0_0) {
        x = 1
    } else {
        x = 2
    }
}
            """
        , testLower
            """
main() {
    let x = 1
    if ("a" < 1) {
        x = 1
    } else {
        x = 2
    }
}
            """
            """
const _string_0_0 = "a"
main() {
    let x = 1
    if (_string_0_0 < 1) {
        x = 1
    } else {
        x = 2
    }
}
            """
        , testLower
            """
main() {
    let x = 1
    if ("a") {
        x = "b"
    } else {
        x = "c"
    }
}
            """
            """
const _string_0_0 = "a"
const _string_0_1 = "b"
const _string_0_2 = "c"
main() {
    let x = 1
    if (_string_0_0) {
        x = _string_0_1
    } else {
        x = _string_0_2
    }
}
            """
        , testLower
            """
main() {
    let x = 1
    loop {
        x = "Hello"
    }
}
            """
            """
const _string_0_0 = "Hello"
main() {
    let x = 1
    loop {
        x = _string_0_0
    }
}
            """
        ]
