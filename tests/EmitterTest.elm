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
import F80.Emitter.Util
import F80.Lower.HoistStringLiterals
import F80.Parser
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


testEmit : String -> String -> Test
testEmit source expected =
    Test.test ("Emitting: " ++ source) <|
        \() ->
            case F80.Parser.parse source of
                Ok decls ->
                    decls
                        |> F80.Lower.HoistStringLiterals.hoist
                        |> F80.Emitter.emit
                        |> Expect.equalLists
                            (expected
                                |> String.trim
                                |> String.split "\n"
                            )

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.describe "F80.Emitter.emit"
        [ globals
        , renderText
        , loops
        ]


globals : Test
globals =
    Test.describe "globals"
        [ testEmit
            "const x = 1"
            """
org 0x8000
end:
    jp end
x db 1
            """
        , testEmit
            "const str = \"Hello World!\""
            """
str_length EQU 12
org 0x8000
end:
    jp end
str db 'Hello World!', 0
            """
        , testEmit
            "const bytes = [1,2,3]"
            """
org 0x8000
end:
    jp end
bytes db 1, 2, 3
            """
        , testEmit
            """
const global = 1
const other = global
            """
            """
other EQU global
org 0x8000
end:
    jp end
global db 1
            """
        , testEmit
            "const binop = 1 + 2"
            """
org 0x8000
end:
    jp end
binop db 1 + 2
            """
        , testEmit
            """const len = String.length("Hello World!")"""
            """
org 0x8000
end:
    jp end
len db 12
            """
        , testEmit
            """
const hello = "Hello"
const len = String.length(hello)
            """
            """
hello_length EQU 5
org 0x8000
end:
    jp end
hello db 'Hello', 0
len db hello_length
            """
        ]


renderText : Test
renderText =
    Test.describe "Render.text()"
        [ testEmit
            """
const hello = "Hello"
main() {
    Render.text(3, 5, hello)
}
            """
            """
AT EQU 0x16
hello_length EQU 5
org 0x8000
main:
    ld hl,0x0305
    ld de,hello
    call renderString
end:
    jp end
renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr renderStringLoop
hello db 'Hello', 0
            """
        , testEmit
            """
main() {
    Render.text(3, 5, "Hello")
}
            """
            """
AT EQU 0x16
_string_0_0_length EQU 5
org 0x8000
main:
    ld hl,0x0305
    ld de,_string_0_0
    call renderString
end:
    jp end
renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr renderStringLoop
_string_0_0 db 'Hello', 0
            """
        ]


loops : Test
loops =
    Test.describe "loops"
        [ testEmit
            """
const hello = "Hello"
main() {
    loop {
        Render.text(0, 0, hello)
    }
}
            """
            """
AT EQU 0x16
hello_length EQU 5
org 0x8000
main:
decl_1_main_0:
    ld hl,0x0000
    ld de,hello
    call renderString
    jp decl_1_main_0
end:
    jp end
renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr renderStringLoop
hello db 'Hello', 0
            """
        ]
