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
        , romCls
        , loops
        , waitForKeypress
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


romCls : Test
romCls =
    Test.describe "ROM.cls()"
        [ testEmit
            """
main() {
    ROM.cls()
}
            """
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    call ROM_CLS
end:
    jp end
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
    call _renderString
end:
    jp end
_renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderStringLoop
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
    call _renderString
end:
    jp end
_renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderStringLoop
_string_0_0 db 'Hello', 0
            """
        , testEmit
            """
main() {
    Render.text(0, 1, "Hello")
    Render.text(2, 3, "World")
}
            """
            """
AT EQU 0x16
_string_0_0_length EQU 5
_string_0_1_length EQU 5
org 0x8000
main:
    ld hl,0x0001
    ld de,_string_0_0
    call _renderString
    ld hl,0x0203
    ld de,_string_0_1
    call _renderString
end:
    jp end
_renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderStringLoop
_string_0_0 db 'Hello', 0
_string_0_1 db 'World', 0
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
    call _renderString
    jp decl_1_main_0
end:
    jp end
_renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderStringLoop
hello db 'Hello', 0
            """
        , testEmit
            """
const hello = "Hello"
main() {
    loop {
        loop {
            Render.text(0, 0, hello)
        }
    }
}
            """
            """
AT EQU 0x16
hello_length EQU 5
org 0x8000
main:
decl_1_main_0:
decl_1_main_0_loop_0:
    ld hl,0x0000
    ld de,hello
    call _renderString
    jp decl_1_main_0_loop_0
    jp decl_1_main_0
end:
    jp end
_renderString:
    ld a, AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderStringLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderStringLoop
hello db 'Hello', 0
            """
        ]


waitForKeypress : Test
waitForKeypress =
    Test.describe "waitForKeypress"
        [ testEmit
            """
main() {
    wait for keypress {
        Key.J -> {}
    }
}
            """
            """
org 0x8000
main:
    jp _wait_0_start
_wait_0_end:
end:
    jp end
_wait_0_start:
_wait_0_JNotPressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_0_JNotPressed
_wait_0_JPressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_0_onJ
    jp _wait_0_JPressed
_wait_0_onJ:
    jp _wait_0_end
            """
        , testEmit
            """
main() {
    wait for keypress {
        Key.J -> { ROM.cls() }
    }
}
            """
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    jp _wait_0_start
_wait_0_end:
end:
    jp end
_wait_0_start:
_wait_0_JNotPressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_0_JNotPressed
_wait_0_JPressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_0_onJ
    jp _wait_0_JPressed
_wait_0_onJ:
    call ROM_CLS
    jp _wait_0_end
            """
        , testEmit
            """
main() {
    wait for keypress {
        Key.J -> {}
        Key.K -> {}
    }
}
            """
            """
org 0x8000
main:
    jp _wait_0_start
_wait_0_end:
end:
    jp end
_wait_0_start:
_wait_0_JKNotPressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_0_JKNotPressed
    bit 2,a
    jp z,_wait_0_JKNotPressed
_wait_0_JKPressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_0_onJ
    bit 2,a
    jp z,_wait_0_onK
    jp _wait_0_JKPressed
_wait_0_onJ:
    jp _wait_0_end
_wait_0_onK:
    jp _wait_0_end
            """
        ]
