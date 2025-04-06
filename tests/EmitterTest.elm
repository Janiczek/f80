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
import F80.Error
import F80.Lower
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


removeCommentsAndEmptyLines : String -> String
removeCommentsAndEmptyLines asm =
    asm
        |> String.lines
        |> List.filter
            (\line ->
                let
                    trimmed =
                        String.trim line
                in
                not
                    (String.isEmpty trimmed
                        || String.startsWith ";" trimmed
                    )
            )
        |> List.map
            (\s ->
                case String.split ";" s of
                    [] ->
                        ""

                    [ x ] ->
                        x

                    code :: _ ->
                        code
                            |> String.trimRight
            )
        |> String.join "\n"


testEmit : String -> String -> Test
testEmit source expected =
    Test.test ("Emitting: " ++ source) <|
        \() ->
            case
                Ok source
                    |> Result.andThen (F80.Parser.parse >> Result.mapError F80.Error.ParserError)
                    |> Result.map F80.Lower.lower
                    |> Result.andThen (F80.Emitter.emit >> Result.mapError F80.Error.TyperError)
            of
                Err err ->
                    Expect.fail ("Failed: " ++ Debug.toString err)

                Ok asm ->
                    asm
                        |> String.join "\n"
                        {-
                           |> (\s ->
                                   let
                                       _ =
                                           Debug.log s "actual"
                                   in
                                   s
                               )
                        -}
                        |> expectEqualMultiline
                            (expected
                                |> String.trim
                                |> removeCommentsAndEmptyLines
                            )


suite : Test
suite =
    Test.describe "F80.Emitter.emit"
        [ globals
        , stmts
        , exprs
        , complex
        , example
        ]


complex : Test
complex =
    Test.describe "complex"
        [ testEmit
            """
// passing multiple args, out of order
fn(a: U8, b: U8): U8 {
    const c = 3
    const d = 4
    return a + b + c + d
}
main() {
    const x = 1
    let y = 2
    const z = fn(y,x)
}
            """
            """
org 0x8000
main:
    ld a,1
    push af     ; -> stack = x         (offset of x = 1)
    ld a,2
    push af     ; -> stack = x,y       (offset of y = 3)
    ld ix,4
    add ix,sp
    ld a,(ix-3) ; y has offset 3
    push af     ; -> stack = x,y,Y     (not tracking offsets for args here)
    ld ix,6
    add ix,sp
    ld a,(ix-1) ; x has offset 1
    push af     ; -> stack = x,y,Y,X   (not tracking offsets for args here)
    call fn
    ld ix,4     ; cleanup 2 args
    add ix,sp
    ld sp,ix    ; -> stack = x,y
    jp _end
_end:
    jp _end
fn:             ; -> stack = A(=Y),B(=X),RET, our base offset is 6 (for now)
    ld a,3
    push af     ; -> stack = A,B,RET,c   (offset of c = 7)
    ld a,4
    push af     ; -> stack = A,B,RET,c,d (offset of d = 9)
    ld ix,10
    add ix,sp
    ld a,(ix-9) ; d has offset 9
    push af     ; -> stack = A,B,RET,c,d,D
    ld ix,12
    add ix,sp
    ld a,(ix-7) ; c has offset 7
    push af     ; -> stack = A,B,RET,c,d,D,C
    ld ix,14
    add ix,sp
    ld a,(ix-3) ; b has offset 3
    push af     ; -> stack = A,B,RET,c,d,D,C,B
    ld ix,16
    add ix,sp
    ld a,(ix-1) ; a has offset 1
    pop bc      ; -> stack = A,B,RET,c,d,D,C
    add b
    pop bc      ; -> stack = A,B,RET,c,d,D
    add b
    pop bc      ; -> stack = A,B,RET,c,d
    add b
    ld ix,4     ; cleanup 2 locals
    add ix,sp
    ld sp,ix    ; -> stack = A,B,RET
    ret
            """
        , testEmit
            """
// lexical scope: main should return 1
fn(): U8 {
    let x = 2
    return x
}
main() {
    let x = 1
    fn()
    const y = x
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    call fn ; we will ignore the return value (in the `a` register)
    ld ix,2
    add ix,sp
    ld a,(ix-1) ; we'll overwrite it with _our_ local `a`
    push af
_end:
    jp _end
fn:
    ld a,2
    push af
    ld ix,4
    add ix,sp
    ld a,(ix-3)
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
            """
        , testEmit
            """
// multiple function calls (next to each other), tests whether we're cleaning up the stack correctly after function calls
foo(a: U8): U8 {
    const x = 7
    return a + x
}
bar(a: U8, b: U8): U8 {
    const y = 8
    return a + b + y
}
baz(a: U8, b: U8, c: U8): U8 {
    const z = 9
    return a + b + c + z
}
main() {
    const x = foo(1) + bar(2,3) + baz(4,5,6)
}
            """
            """
org 0x8000
main:
    ld a,4
    push af
    ld a,5
    push af
    ld a,6
    push af
    call baz
    ld ix,6
    add ix,sp
    ld sp,ix
    push af
    ld a,2
    push af
    ld a,3
    push af
    call bar
    ld ix,4
    add ix,sp
    ld sp,ix
    push af
    ld a,1
    push af
    call foo
    ld ix,2
    add ix,sp
    ld sp,ix
    pop bc
    add b
    pop bc
    add b
    jp _end
_end:
    jp _end
bar:
    ld a,8
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-7)
    push af
    ld ix,10
    add ix,sp
    ld a,(ix-3)
    push af
    ld ix,12
    add ix,sp
    ld a,(ix-1)
    pop bc
    add b
    pop bc
    add b
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
baz:
    ld a,9
    push af
    ld ix,10
    add ix,sp
    ld a,(ix-9)
    push af
    ld ix,12
    add ix,sp
    ld a,(ix-5)
    push af
    ld ix,14
    add ix,sp
    ld a,(ix-3)
    push af
    ld ix,16
    add ix,sp
    ld a,(ix-1)
    pop bc
    add b
    pop bc
    add b
    pop bc
    add b
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
foo:
    ld a,7
    push af
    ld ix,6
    add ix,sp
    ld a,(ix-5)
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-1)
    pop bc
    add b
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
            """
        , testEmit
            """
// nested function calls
bar(y: U8, a: U8): U8 {
    let z = 4
    return a + z + y
}
foo(a: U8): U8 {
    let y = 3
    return a + y + bar(y,a)
}
main() {
    let x = 1
    let y = foo(x) + 2
    // -> 1+3+1+4+3+2 = 14
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    ld a,2
    push af
    ld ix,4
    add ix,sp
    ld a,(ix-1)
    push af
    call foo
    ld ix,2
    add ix,sp
    ld sp,ix
    pop bc
    add b
    jp _end
_end:
    jp _end
bar:
    ld a,4
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-1)
    push af
    ld ix,10
    add ix,sp
    ld a,(ix-7)
    push af
    ld ix,12
    add ix,sp
    ld a,(ix-3)
    pop bc
    add b
    pop bc
    add b
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
foo:
    ld a,3
    push af
    ld ix,6
    add ix,sp
    ld a,(ix-5)
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-1)
    push af
    call bar
    ld ix,4
    add ix,sp
    ld sp,ix
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-5)
    push af
    ld ix,10
    add ix,sp
    ld a,(ix-1)
    pop bc
    add b
    pop bc
    add b
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
            """
        ]


stmts : Test
stmts =
    Test.describe "stmts"
        [ loops
        , waitForKeypress
        , ifStmts
        , returns
        , consts
        , lets
        , assignStmts
        , callStmts
        ]


assignStmts : Test
assignStmts =
    Test.describe "assign stmts"
        [ testEmit
            """
main() {
    let x = 1
    x = 2
}
            """
            """
org 0x8000
main:
    ld a,1
    push af ; save var on the stack
    ld a,2

    ; overwrite var on the stack
    ld ix,2
    add ix,sp
    ld (ix-1),a
_end:
    jp _end
            """
        , testEmit
            """
main() {
    let x = 1
    const y = x
    x = 2
    return x + y // y should still be 1, so we should return 3
}
            """
            """
org 0x8000
main:
    ld a,1

    push af ; save x on the stack

    ; load x from the stack
    ld ix,2
    add ix,sp
    ld a,(ix-1)

    push af ; save y on the stack

    ld a,2

    ; overwrite x on the stack
    ld ix,4
    add ix,sp
    ld (ix-1),a

    ; load y from the stack
    ld ix,4
    add ix,sp
    ld a,(ix-3)

    push af

    ; load x from the stack
    ld ix,6
    add ix,sp
    ld a,(ix-1)

    pop bc
    add b
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
foo(): U8 {
    let x = 1
    x += 2
    return x // we should return 3
}

main() {
    foo()
}
            """
            """
org 0x8000
main:
    call foo
_end:
    jp _end
foo:
    ld a,1
    push af
    ld a,2
    ld b,a
    ld ix,4
    add ix,sp
    ld a,(ix-3)
    add b
    ld (ix-3),a
    ld ix,4
    add ix,sp
    ld a,(ix-3)
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
            """
        , testEmit
            """
main() {
    let x = 3
    x -= 1
    return x // we should return 2
}
            """
            """
org 0x8000
main:
    ld a,3
    push af
    ld a,1
    ld b,a
    ld ix,2
    add ix,sp
    ld a,(ix-1)
    sub b
    ld (ix-1),a
    ld ix,2
    add ix,sp
    ld a,(ix-1)
    jp _end
_end:
    jp _end
            """
        ]


consts : Test
consts =
    Test.describe "consts"
        [ testEmit
            """
main() {
    const x = 1
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
_end:
    jp _end
            """
        , testEmit
            """
main() {
    const x = 1
    const y = 2
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    ld a,2
    push af
_end:
    jp _end
            """
        , testEmit
            """
// Shadowing const inside an if block
main() {
    const x = 1
    const y = foo()
    return x + y     // 1 + 9 = 10
}
foo() {
    const x = 2
    let y = 0
    if (true) {
        const x = 3
        y += bar()  // 0 + 4 = 4
        y += x      // 4 + 3 = 7
    }
    return x + y    // 2 + 7 = 9
}
bar() {
    const x = 4
    return x        // 4
}
            """
            """
org 0x8000
main:
    ld a,1
    push af     ; save x at offset 1
    call foo
    push af     ; save y at offset 3
    ld ix,4
    add ix,sp
    ld a,(ix-3) ; load y
    push af
    ld ix,6
    add ix,sp
    ld a,(ix-1) ; load x
    pop bc
    add b
    ; no cleanup of locals in main
    jp _end
_end:
    jp _end
bar:
    ld a,4
    push af     ; save x at offset 3 (offset 1 = CALL return address)
    ld ix,4
    add ix,sp
    ld a,(ix-3) ; load x
    ; cleanup bar() (1 local)
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
foo:
    ld a,2      ; save x at offset 3 (offset 1 = CALL return address)
    push af
    ld a,0      ; save y at offset 5
    push af
    ld a,255    ; IF
    cp 255
    jp nz,_ifstmt_decl_main_foo_2_end
    ; THEN --------------------------
    ld a,3
    push af     ; save x at offset 7
    call bar
    ld b,a
    ld ix,8
    add ix,sp
    ld a,(ix-5) ; load y
    add b
    ld (ix-5),a  ; y += bar()
    ld ix,8
    add ix,sp
    ld a,(ix-7) ; load x (inside if(true))
    ld b,a
    ld ix,8
    add ix,sp
    ld a,(ix-5) ; load y
    add b
    ld (ix-5),a  ; y += x
    ; cleanup block (1 local)
    ld ix,2
    add ix,sp
    ld sp,ix
    ; END THEN --------------------------
    ; NO ELSE ---------------------------
_ifstmt_decl_main_foo_2_end:
    ld ix,6
    add ix,sp
    ld a,(ix-5) ; load y
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-3) ; load x (in foo())
    pop bc
    add b
    ; cleanup foo() (2 locals)
    ld ix,4
    add ix,sp
    ld sp,ix
    ret
            """
        ]


lets : Test
lets =
    Test.describe "lets"
        [ testEmit
            """
main() {
    let x = 1
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
_end:
    jp _end
            """
        , testEmit
            """
main() {
    let x = 1
    let y = 2
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    ld a,2
    push af
_end:
    jp _end
            """
        ]


callStmts : Test
callStmts =
    Test.describe "call stmts"
        [ renderTextStmt
        , romClsStmt
        , generic0ArgCallStmt
        , generic1ArgCallStmt
        , generic2ArgCallStmt
        , generic5ArgCallStmt
        ]


generic0ArgCallStmt : Test
generic0ArgCallStmt =
    Test.describe "generic 0-arg call stmt"
        [ testEmit
            """
fn(): U8{
    return 1
}
main(){
    fn()
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
fn(a: U8): U8 {
    return a
}
main(){
    fn(1)
}
            """
            """
org 0x8000
main:
    ld a,1
    push af      ; -> stack = 1  (not tracking offsets for args here)
    call fn
    ld ix,2      ; -> cleanup of call args, stack = <empty>
    add ix,sp
    ld sp,ix
_end:
    jp _end
fn:              ; -> stack = A(=1),RET, our base offset is 4, offset of a = 1
    ld ix,4
    add ix,sp
    ld a,(ix-1)  ; a has offset 1
    ret
            """
        ]


generic2ArgCallStmt : Test
generic2ArgCallStmt =
    Test.describe "generic 2-arg call stmt"
        [ testEmit
            """
fn(a: U8, b: U8): U8 {
    return a + b
}
main(){
    fn(1,2)
}
            """
            """
org 0x8000
main:
    ld a,1
    push af      ; -> stack = 1   (not tracking offsets for args here)
    ld a,2
    push af      ; -> stack = 1,2 (not tracking offsets for args here)
    call fn
    ld ix,4      ; -> cleanup of call args, stack = <empty>
    add ix,sp
    ld sp,ix
_end:
    jp _end
fn:              ; -> stack = A(=1),B(=2),RET, our base offset is 6, offset of a = 1, offset of b = 3
    ld ix,6
    add ix,sp
    ld a,(ix-3)  ; b has offset 3
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-1)  ; a has offset 1
    pop bc
    add b
    ret
            """
        ]


generic5ArgCallStmt : Test
generic5ArgCallStmt =
    Test.describe "generic 5-arg call stmt"
        [ testEmit
            """
fn(a: U8, b: U8, c: U8, d: U8, e: U8): U8 {
    return a + b + c + d + e
}
main(){
    fn(1,2,3,4,5)
}
            """
            """
org 0x8000
main:
    ld a,1
    push af      ; -> stack = 1         (not tracking offsets for args here)
    ld a,2
    push af      ; -> stack = 1,2       (not tracking offsets for args here)
    ld a,3
    push af      ; -> stack = 1,2,3     (not tracking offsets for args here)
    ld a,4
    push af      ; -> stack = 1,2,3,4   (not tracking offsets for args here)
    ld a,5
    push af      ; -> stack = 1,2,3,4,5 (not tracking offsets for args here)
    call fn
    ld ix,10      ; -> cleanup of call args, stack = <empty>
    add ix,sp
    ld sp,ix
_end:
    jp _end
fn:              ; -> stack = A(=1),B(=2),C(=3),D(=4),E(=5),RET, our base offset is 12, offsets of a,b,c,d,e are 1,3,5,7,9
    ld ix,12
    add ix,sp
    ld a,(ix-9)  ; e has offset 9
    push af
    ld ix,14
    add ix,sp
    ld a,(ix-7)  ; d has offset 7
    push af
    ld ix,16
    add ix,sp
    ld a,(ix-5)  ; c has offset 5
    push af
    ld ix,18
    add ix,sp
    ld a,(ix-3)  ; b has offset 3
    push af
    ld ix,20
    add ix,sp
    ld a,(ix-1)  ; a has offset 1
    pop bc
    add b
    pop bc
    add b
    pop bc
    add b
    pop bc
    add b
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
    call _renderText
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderTextLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderTextLoop
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
    call _renderText
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderTextLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderTextLoop
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
    call _renderText
    ld hl,515
    ld de,_string_0_1
    call _renderText
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderTextLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderTextLoop
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
    jp nz,_ifstmt_decl_main_stmt_0_end
    call ROM_CLS
_ifstmt_decl_main_stmt_0_end:
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
    jp nz,_ifstmt_decl_main_stmt_0_else
    call ROM_CLS
    jp _ifstmt_decl_main_stmt_0_end
_ifstmt_decl_main_stmt_0_else:
    call ROM_CLS
_ifstmt_decl_main_stmt_0_end:
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
    jp nc,_lt_decl_main_stmt_0_cond_binop_onLT
    ld a,0
    jp _lt_decl_main_stmt_0_cond_binop_end
_lt_decl_main_stmt_0_cond_binop_onLT:
    ld a,255
_lt_decl_main_stmt_0_cond_binop_end:
    cp 255
    jp nz,_ifstmt_decl_main_stmt_0_end
    call ROM_CLS
_ifstmt_decl_main_stmt_0_end:
_end:
    jp _end
            """
        , testEmit
            """
main() {
    if (1 > 2) {
        ROM.clearScreen()
    }
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
ROM_CLS EQU 0x0daf
org 0x8000
main:
    ld a,1
    push af
    ld a,2
    pop bc
    cp b
    jp nc,_gt_decl_main_stmt_0_cond_binop_onGT
    ld a,0
    jp _gt_decl_main_stmt_0_cond_binop_end
_gt_decl_main_stmt_0_cond_binop_onGT:
    ld a,255
_gt_decl_main_stmt_0_cond_binop_end:
    cp 255
    jp nz,_ifstmt_decl_main_stmt_0_end
    call ROM_CLS
_ifstmt_decl_main_stmt_0_end:
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
    jp nz,_ifstmt_decl_main_stmt_0_end
    call ROM_CLS
_ifstmt_decl_main_stmt_0_end:
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
decl_main_stmt_0:
    ld hl,0
    ld de,hello
    call _renderText
    jp decl_main_stmt_0
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderTextLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderTextLoop
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
decl_main_stmt_0:
decl_main_stmt_0_loop_stmt_0:
    ld hl,0
    ld de,hello
    call _renderText
    jp decl_main_stmt_0_loop_stmt_0
    jp decl_main_stmt_0
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderTextLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderTextLoop
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
    jp _wait_decl_main_stmt_0_start
_wait_decl_main_stmt_0_end:
_end:
    jp _end
_wait_decl_main_stmt_0_start:
_wait_decl_main_stmt_0_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_0_none_pressed
_wait_decl_main_stmt_0_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_0_onJ
    jp _wait_decl_main_stmt_0_any_pressed
_wait_decl_main_stmt_0_onJ:
    jp _wait_decl_main_stmt_0_end
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
    jp _wait_decl_main_stmt_0_start
_wait_decl_main_stmt_0_end:
_end:
    jp _end
_wait_decl_main_stmt_0_start:
_wait_decl_main_stmt_0_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_0_none_pressed
_wait_decl_main_stmt_0_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_0_onJ
    jp _wait_decl_main_stmt_0_any_pressed
_wait_decl_main_stmt_0_onJ:
    call ROM_CLS
    jp _wait_decl_main_stmt_0_end
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
    jp _wait_decl_main_stmt_0_start
_wait_decl_main_stmt_0_end:
_end:
    jp _end
_wait_decl_main_stmt_0_start:
_wait_decl_main_stmt_0_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_0_none_pressed
    bit 2,a
    jp z,_wait_decl_main_stmt_0_none_pressed
_wait_decl_main_stmt_0_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_0_onJ
    bit 2,a
    jp z,_wait_decl_main_stmt_0_onK
    jp _wait_decl_main_stmt_0_any_pressed
_wait_decl_main_stmt_0_onJ:
    jp _wait_decl_main_stmt_0_end
_wait_decl_main_stmt_0_onK:
    jp _wait_decl_main_stmt_0_end
            """
        ]


exprs : Test
exprs =
    Test.describe "exprs"
        [ ints
        , strings
        , bools
        , vars
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
        , generic2ArgCallExpr
        , generic5ArgCallExpr
        ]


generic0ArgCallExpr : Test
generic0ArgCallExpr =
    Test.describe "generic 0-arg call expr"
        [ testEmit
            """
main(){
    return fn()
}
fn(): U8 {
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
fn(a: U8): U8 {
    return a
}
            """
            """
org 0x8000
main:
    ld a,1
    push af      ; -> stack = 1  (not tracking offsets for args here)
    call fn
    ld ix,2      ; -> stack = <empty>
    add ix,sp
    ld sp,ix
    jp _end
_end:
    jp _end
fn:              ; -> stack = A(=1),RET, our base offset is 4, offset of a = 1
    ld ix,4
    add ix,sp
    ld a,(ix-1)  ; a has offset 1
    ret
            """
        ]


generic2ArgCallExpr : Test
generic2ArgCallExpr =
    Test.describe "generic 2-arg call expr"
        [ testEmit
            """
main(){
    const x = fn(1,2)
}
fn(a: U8, b: U8): U8 {
    return a + b
}
            """
            """
org 0x8000
main:
    ld a,1
    push af      ; -> stack = 1   (not tracking offsets for args here)
    ld a,2
    push af      ; -> stack = 1,2 (not tracking offsets for args here)
    call fn
    ld ix,4      ; -> cleanup of call args, stack = <empty>
    add ix,sp
    ld sp,ix
    jp _end
_end:
    jp _end
fn:              ; -> stack = A(=1),B(=2),RET, our base offset is 6, offset of a = 1, offset of b = 3
    ld ix,6
    add ix,sp
    ld a,(ix-3)  ; b has offset 3
    push af
    ld ix,8
    add ix,sp
    ld a,(ix-1)  ; a has offset 1
    pop bc
    add b
    ret
            """
        ]


generic5ArgCallExpr : Test
generic5ArgCallExpr =
    Test.describe "generic 5-arg call expr"
        [ testEmit
            """
fn(a: U8, b: U8, c: U8, d: U8, e: U8): U8 {
    return a + b + c + d + e
}
main(){
    const x = fn(1,2,3,4,5)
}
            """
            """
org 0x8000
main:
    ld a,1
    push af      ; -> stack = 1         (not tracking offsets for args here)
    ld a,2
    push af      ; -> stack = 1,2       (not tracking offsets for args here)
    ld a,3
    push af      ; -> stack = 1,2,3     (not tracking offsets for args here)
    ld a,4
    push af      ; -> stack = 1,2,3,4   (not tracking offsets for args here)
    ld a,5
    push af      ; -> stack = 1,2,3,4,5 (not tracking offsets for args here)
    call fn
    ld ix,10      ; -> cleanup of call args, stack = <empty>
    add ix,sp
    ld sp,ix
    jp _end
_end:
    jp _end
fn:              ; -> stack = A(=1),B(=2),C(=3),D(=4),E(=5),RET, our base offset is 12, offsets of a,b,c,d,e are 1,3,5,7,9
    ld ix,12
    add ix,sp
    ld a,(ix-9)  ; e has offset 9
    push af
    ld ix,14
    add ix,sp
    ld a,(ix-7)  ; d has offset 7
    push af
    ld ix,16
    add ix,sp
    ld a,(ix-5)  ; c has offset 5
    push af
    ld ix,18
    add ix,sp
    ld a,(ix-3)  ; b has offset 3
    push af
    ld ix,20
    add ix,sp
    ld a,(ix-1)  ; a has offset 1
    pop bc
    add b
    pop bc
    add b
    pop bc
    add b
    pop bc
    add b
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
    call _renderText
    jp _end
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,l
    rst 0x10
    ld a,h
    rst 0x10
_renderTextLoop:
    ld a,(de)
    cp 0
    ret z
    rst 0x10
    inc de
    jr _renderTextLoop
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
    ld de,_string_0_0
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
    const x = !true
}
            """
            """
org 0x8000
main:
    ld a,255
    neg
    push af
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
    jp nc,_gt_decl_main_stmt_0_binop_onGT
    ld a,0
    jp _gt_decl_main_stmt_0_binop_end
_gt_decl_main_stmt_0_binop_onGT:
    ld a,255
_gt_decl_main_stmt_0_binop_end:
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
    jp nc,_lt_decl_main_stmt_0_binop_onLT
    ld a,0
    jp _lt_decl_main_stmt_0_binop_end
_lt_decl_main_stmt_0_binop_onLT:
    ld a,255
_lt_decl_main_stmt_0_binop_end:
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


vars : Test
vars =
    Test.describe "vars"
        [ testEmit
            """
main() {
    const x = 1
    return x
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    ld ix,2
    add ix,sp
    ld a,(ix-1)
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    const x = 1
    const y = 2
    return x
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    ld a,2
    push af
    ld ix,4
    add ix,sp
    ld a,(ix-1)
    jp _end
_end:
    jp _end
            """
        , testEmit
            """
main() {
    const x = 1
    const y = 2
    return y
}
            """
            """
org 0x8000
main:
    ld a,1
    push af
    ld a,2
    push af
    ld ix,4
    add ix,sp
    ld a,(ix-3)
    jp _end
_end:
    jp _end
            """
        ]


ifExprs : Test
ifExprs =
    Test.describe "if exprs"
        [ testEmit
            """
main() {
    const x = if (true) 5 else 6
}
            """
            -- TODO we can optimize this away later, for now this is translated verbatim
            """
org 0x8000
main:
    ld a,255
    cp 255
    jp nz,_ifexpr_decl_main_stmt_0_else
    ld a,5
    jp _ifexpr_decl_main_stmt_0_end
_ifexpr_decl_main_stmt_0_else:
    ld a,6
_ifexpr_decl_main_stmt_0_end:
    push af
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
    jp nz,_ifexpr_decl_main_fn_0_else
    ld a,5
    jp _ifexpr_decl_main_fn_0_end
_ifexpr_decl_main_fn_0_else:
    ld a,6
_ifexpr_decl_main_fn_0_end:
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
    jp nz,_ifexpr_decl_main_fn_0_else
    ld a,0
    jp _ifexpr_decl_main_fn_0_end
_ifexpr_decl_main_fn_0_else:
    ld a,0
    cp 255
    jp nz,_ifexpr_decl_main_fn_0_if_else_else
    ld a,1
    jp _ifexpr_decl_main_fn_0_if_else_end
_ifexpr_decl_main_fn_0_if_else_else:
    ld a,2
_ifexpr_decl_main_fn_0_if_else_end:
_ifexpr_decl_main_fn_0_end:
    ret
            """
        ]


example : Test
example =
    Test.describe "example"
        [ testEmit
            Example.sourceCode
            Example.assembly
        ]
