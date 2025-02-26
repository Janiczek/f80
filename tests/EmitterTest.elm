module EmitterTest exposing (suite)

import Ansi.Color
import Diff
import Diff.ToString
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
import F80.Lower.AddImplicitReturns
import F80.Lower.HoistStringLiterals
import F80.Parser
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


{-| Taken straight from docs of miniBill/elm-diff
-}
expectEqualMultiline : String -> String -> Expectation
expectEqualMultiline expected actual =
    if expected == actual then
        Expect.pass

    else
        let
            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:"
        in
        Expect.fail
            (header
                ++ "\n"
                ++ (Diff.diffLinesWith
                        (Diff.defaultOptions
                            |> Diff.ignoreLeadingWhitespace
                        )
                        expected
                        actual
                        |> Diff.ToString.diffToString { context = 4, color = True }
                   )
            )


testEmit : String -> String -> Test
testEmit source expected =
    Test.test ("Emitting: " ++ source) <|
        \() ->
            case F80.Parser.parse source of
                Ok decls ->
                    decls
                        |> F80.Lower.HoistStringLiterals.hoist
                        |> F80.Lower.AddImplicitReturns.add
                        |> F80.Emitter.emit
                        |> String.join "\n"
                        |> expectEqualMultiline
                            (expected
                                |> String.trim
                            )

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.describe "F80.Emitter.emit"
        [ globals
        , stmts
        , exprs
        ]


stmts : Test
stmts =
    Test.describe "stmts"
        [ loops
        , waitForKeypress
        , ifStmts
        , returns
        , Test.todo "const stmts"
        , Test.todo "let stmts"
        , Test.todo "assign stmts"
        , callStmts
        ]


callStmts : Test
callStmts =
    Test.describe "call stmts"
        [ renderTextStmt
        , romClsStmt
        , generic0ArgCallStmt
        , generic1ArgCallStmt
        , Test.todo "generic 2-arg call stmt"
        , Test.todo "generic 3-arg call stmt"
        , Test.todo "generic 4-arg call stmt"
        , Test.todo "generic 5-arg call stmt"
        ]


generic0ArgCallStmt : Test
generic0ArgCallStmt =
    Test.describe "generic 0-arg call stmt"
        [ testEmit
            """
main(){
    fn()
}
fn(){
    return 1
}
            """
            """
org 0x8000
main:
    call fn
_end:
    jp _end
fn:
    ld a,1
    ret
            """
        ]


generic1ArgCallStmt : Test
generic1ArgCallStmt =
    Test.describe "generic 1-arg call stmt"
        [ testEmit
            """
main(){
    fn(1)
}
fn(a){
    return a
}
            """
            -- TODO optimize away ld a,a
            """
org 0x8000
main:
    ld a,1
    push af
    call fn
_end:
    jp _end
fn:
    pop af
    ld a,a
    ret
            """
        ]


returns : Test
returns =
    Test.describe "returns"
        [ testEmit
            """
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
            """
        , testEmit
            """
main() {
    return
}
            """
            -- TODO optimize away jp _end before _end:
            """
org 0x8000
main:
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    return 1
}
            """
            """
org 0x8000
main:
    ld a,1
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
fn() {
}
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ret
            """
        , testEmit
            """
fn() {
    return 1
}
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,1
    ret
            """
        ]


globals : Test
globals =
    Test.describe "globals"
        [ testEmit
            "const x = 1"
            """
org 0x8000
_end:
    jp _end
x db 1
            """
        , testEmit
            "const str = \"Hello World!\""
            """
str_length EQU 12
org 0x8000
_end:
    jp _end
str db 'Hello World!', 0
            """
        , testEmit
            "const bytes = [1,2,3]"
            """
org 0x8000
_end:
    jp _end
bytes db 1, 2, 3
            """
        , testEmit
            "const bool = true"
            """
org 0x8000
_end:
    jp _end
bool db 255
            """
        , testEmit
            "const bool = false"
            """
org 0x8000
_end:
    jp _end
bool db 0
            """
        , testEmit
            """
const global = 1
const other = global
            """
            """
other EQU global
org 0x8000
_end:
    jp _end
global db 1
            """
        , testEmit
            "const binop = 1 + 2"
            """
org 0x8000
_end:
    jp _end
binop db 1 + 2
            """
        , testEmit
            "const binop = 1 - 2"
            """
org 0x8000
_end:
    jp _end
binop db 1 - 2
            """
        , testEmit
            "const binop = 1 < 2"
            """
org 0x8000
_end:
    jp _end
binop db 1 < 2
            """
        , testEmit
            "const binop = 1 > 2"
            """
org 0x8000
_end:
    jp _end
binop db 1 > 2
            """
        , testEmit
            "const unop = -5"
            """
org 0x8000
_end:
    jp _end
unop db - 5
            """
        , testEmit
            "const unop = !true"
            """
org 0x8000
_end:
    jp _end
unop db NOT 255
            """
        , testEmit
            """const len = String.length("Hello World!")"""
            """
org 0x8000
_end:
    jp _end
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
_end:
    jp _end
hello db 'Hello', 0
len db hello_length
            """
        ]


romClsStmt : Test
romClsStmt =
    Test.describe "ROM.clearScreen() stmt"
        [ testEmit
            """
main() {
    ROM.clearScreen()
}
            """
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    call ROM_CLS
_end:
    jp _end
            """
        ]


renderTextStmt : Test
renderTextStmt =
    Test.describe "Render.text() stmt"
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
    ld hl,773
    ld de,hello
    call _renderString
_end:
    jp _end
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
    ld hl,773
    ld de,_string_0_0
    call _renderString
_end:
    jp _end
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
    ld hl,1
    ld de,_string_0_0
    call _renderString
    ld hl,515
    ld de,_string_0_1
    call _renderString
_end:
    jp _end
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


ifStmts : Test
ifStmts =
    Test.describe "if stmts"
        [ testEmit
            """
main() {
    if (true) {
        ROM.clearScreen()
    }
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    ld a,255
    cp 255
    jp nz,_ifstmt_decl_0_main_0_end
    call ROM_CLS
_ifstmt_decl_0_main_0_end:
_end:
    jp _end
            """
        , testEmit
            """
main() {
    if (true) {
        ROM.clearScreen()
    } else {
        ROM.clearScreen()
    }
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    ld a,255
    cp 255
    jp nz,_ifstmt_decl_0_main_0_else
    call ROM_CLS
    jp _ifstmt_decl_0_main_0_end
_ifstmt_decl_0_main_0_else:
    call ROM_CLS
_ifstmt_decl_0_main_0_end:
_end:
    jp _end
            """
        , testEmit
            """
main() {
    if (1 < 2) {
        ROM.clearScreen()
    }
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    ld a,2
    push af
    ld a,1
    pop bc
    cp b
    jp nc,_lt_decl_0_main_0_cond_onLT
    ld a,0
    jp _lt_decl_0_main_0_cond_end
_lt_decl_0_main_0_cond_onLT:
    ld a,255
_lt_decl_0_main_0_cond_end:
    cp 255
    jp nz,_ifstmt_decl_0_main_0_end
    call ROM_CLS
_ifstmt_decl_0_main_0_end:
_end:
    jp _end
            """
        , testEmit
            """
fn() {
    return true
}
main() {
    if (fn()) {
        ROM.clearScreen()
    }
}
            """
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    call fn
    cp 255
    jp nz,_ifstmt_decl_1_main_0_end
    call ROM_CLS
_ifstmt_decl_1_main_0_end:
_end:
    jp _end
fn:
    ld a,255
    ret
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
    ld hl,0
    ld de,hello
    call _renderString
    jp decl_1_main_0
_end:
    jp _end
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
    ld hl,0
    ld de,hello
    call _renderString
    jp decl_1_main_0_loop_0
    jp decl_1_main_0
_end:
    jp _end
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
    jp _wait_decl_0_main_0_start
_wait_decl_0_main_0_end:
_end:
    jp _end
_wait_decl_0_main_0_start:
_wait_decl_0_main_0_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_0_main_0_none_pressed
_wait_decl_0_main_0_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_0_main_0_onJ
    jp _wait_decl_0_main_0_any_pressed
_wait_decl_0_main_0_onJ:
    jp _wait_decl_0_main_0_end
            """
        , testEmit
            """
main() {
    wait for keypress {
        Key.J -> { ROM.clearScreen() }
    }
}
            """
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    jp _wait_decl_0_main_0_start
_wait_decl_0_main_0_end:
_end:
    jp _end
_wait_decl_0_main_0_start:
_wait_decl_0_main_0_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_0_main_0_none_pressed
_wait_decl_0_main_0_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_0_main_0_onJ
    jp _wait_decl_0_main_0_any_pressed
_wait_decl_0_main_0_onJ:
    call ROM_CLS
    jp _wait_decl_0_main_0_end
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
    jp _wait_decl_0_main_0_start
_wait_decl_0_main_0_end:
_end:
    jp _end
_wait_decl_0_main_0_start:
_wait_decl_0_main_0_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_0_main_0_none_pressed
    bit 2,a
    jp z,_wait_decl_0_main_0_none_pressed
_wait_decl_0_main_0_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_0_main_0_onJ
    bit 2,a
    jp z,_wait_decl_0_main_0_onK
    jp _wait_decl_0_main_0_any_pressed
_wait_decl_0_main_0_onJ:
    jp _wait_decl_0_main_0_end
_wait_decl_0_main_0_onK:
    jp _wait_decl_0_main_0_end
            """
        ]


exprs : Test
exprs =
    Test.describe "exprs"
        [ ints
        , strings
        , bools
        , Test.todo "var exprs"
        , binOps
        , unaryOps
        , callExprs
        , ifExprs
        ]


callExprs : Test
callExprs =
    Test.describe "call Exprs"
        [ renderTextExpr
        , romClsExpr
        , generic0ArgCallExpr
        , generic1ArgCallExpr
        , Test.todo "generic 2-arg call expr"
        , Test.todo "generic 3-arg call expr"
        , Test.todo "generic 4-arg call expr"
        , Test.todo "generic 5-arg call expr"
        ]


generic0ArgCallExpr : Test
generic0ArgCallExpr =
    Test.describe "generic 0-arg call expr"
        [ testEmit
            """
main(){
    return fn()
}
fn(){
    return 1
}
            """
            """
org 0x8000
main:
    call fn
    jp _end
_end:
    jp _end
fn:
    ld a,1
    ret
            """
        ]


generic1ArgCallExpr : Test
generic1ArgCallExpr =
    Test.describe "generic 1-arg call expr"
        [ testEmit
            """
main(){
    return fn(1)
}
fn(a){
    return a
}
            """
            -- TODO optimize away ld a,a
            """
org 0x8000
main:
    ld a,1
    push af
    call fn
    jp _end
_end:
    jp _end
fn:
    pop af
    ld a,a
    ret
            """
        ]


romClsExpr : Test
romClsExpr =
    Test.describe "ROM.clearScreen() expr"
        [ testEmit
            """
main() {
    return ROM.clearScreen()
}
            """
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    call ROM_CLS
    jp _end
_end:
    jp _end
            """
        ]


renderTextExpr : Test
renderTextExpr =
    Test.describe "Render.text() expr"
        [ testEmit
            """
const hello = "Hello"
main() {
    return Render.text(3, 5, hello)
}
            """
            """
AT EQU 0x16
hello_length EQU 5
org 0x8000
main:
    ld hl,773
    ld de,hello
    call _renderString
    jp _end
_end:
    jp _end
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


strings : Test
strings =
    Test.describe "strings"
        [ -- String literals do get hoisted to global vars and so we use them as vars.
          testEmit
            """
fn() {
    return "Hello"
}
main() {
}
            """
            """
_string_0_0_length EQU 5
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,_string_0_0
    ret
_string_0_0 db 'Hello', 0
            """
        ]


unaryOps : Test
unaryOps =
    Test.describe "unary ops"
        [ testEmit
            """
main() {
    return -1
}
            """
            """
org 0x8000
main:
    ld a,1
    neg
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    return !true
}
            """
            """
org 0x8000
main:
    ld a,255
    neg
    jp _end
_end:
    jp _end
            """
        ]


binOps : Test
binOps =
    Test.describe "binary ops"
        [ testEmit
            """
main() {
    return 10 + 20
}
            """
            """
org 0x8000
main:
    ld a,20
    push af
    ld a,10
    pop bc
    add b
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    return 10 - 20
}
            """
            """
org 0x8000
main:
    ld a,20
    push af
    ld a,10
    pop bc
    sub b
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    return 10 > 20
}
            """
            """
org 0x8000
main:
    ld a,10
    push af
    ld a,20
    pop bc
    cp b
    jp nc,_gt_decl_0_main_0_return_onGT
    ld a,0
    jp _gt_decl_0_main_0_return_end
_gt_decl_0_main_0_return_onGT:
    ld a,255
_gt_decl_0_main_0_return_end:
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    return 10 < 20
}
            """
            """
org 0x8000
main:
    ld a,20
    push af
    ld a,10
    pop bc
    cp b
    jp nc,_lt_decl_0_main_0_return_onLT
    ld a,0
    jp _lt_decl_0_main_0_return_end
_lt_decl_0_main_0_return_onLT:
    ld a,255
_lt_decl_0_main_0_return_end:
    jp _end
_end:
    jp _end
            """
        ]


ints : Test
ints =
    Test.describe "ints"
        [ testEmit
            """
fn() {
    return 1
}
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,1
    ret
            """
        , testEmit
            """
fn() {
    return 42
}
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,42
    ret
            """
        ]


bools : Test
bools =
    Test.describe "bool"
        [ testEmit
            """
fn() {
    return true
}
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,255
    ret
            """
        , testEmit
            """
fn() {
    return false
}
main() {
}
            """
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,0
    ret
            """
        ]


ifExprs : Test
ifExprs =
    Test.describe "if exprs"
        [ testEmit
            """
main() {
    return (if (true) 5 else 6)
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
org 0x8000
main:
    ld a,255
    cp 255
    jp nz,_ifexpr_decl_0_main_0_return_else
    ld a,5
    jp _ifexpr_decl_0_main_0_return_end
_ifexpr_decl_0_main_0_return_else:
    ld a,6
_ifexpr_decl_0_main_0_return_end:
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {}
fn() {
    return (if (true) 5 else 6)
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,255
    cp 255
    jp nz,_ifexpr_decl_1_fn_0_return_else
    ld a,5
    jp _ifexpr_decl_1_fn_0_return_end
_ifexpr_decl_1_fn_0_return_else:
    ld a,6
_ifexpr_decl_1_fn_0_return_end:
    ret
            """
        , testEmit
            """
main() {}
fn() {
    return (if (true) 0 else (if (false) 1 else 2))
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
org 0x8000
main:
_end:
    jp _end
fn:
    ld a,255
    cp 255
    jp nz,_ifexpr_decl_1_fn_0_return_else
    ld a,0
    jp _ifexpr_decl_1_fn_0_return_end
_ifexpr_decl_1_fn_0_return_else:
    ld a,0
    cp 255
    jp nz,_ifexpr_decl_1_fn_0_return_else_else
    ld a,1
    jp _ifexpr_decl_1_fn_0_return_else_end
_ifexpr_decl_1_fn_0_return_else_else:
    ld a,2
_ifexpr_decl_1_fn_0_return_else_end:
_ifexpr_decl_1_fn_0_return_end:
    ret
            """
        ]
