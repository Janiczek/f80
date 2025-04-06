module Example exposing
    ( assembly
    , ast
    , sourceCode
    )

import F80.AST
    exposing
        ( BinOp(..)
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Param
        , Stmt(..)
        , Value(..)
        )
import F80.Type


sourceCode : String
sourceCode =
    """
renderStaticText() {
  Render.text(5, 5, "Counter: ")
  Render.text(5, 7, "Press J (-) or K (+)")
}

renderCounter(counter: U8) {
  Render.text(14, 5, "   ")
  const counterStr = String.fromU8(counter)
  Render.text(14, 5, counterStr)
}

main() {
  ROM.clearScreen()
  let counter = 0
  renderStaticText()
  loop {
    renderCounter(counter)
    wait for keypress {
      Key.J -> { counter -= 1 }
      Key.K -> { counter += 1 }
    }
  }
}
"""


ast : F80.AST.Program
ast =
    [ FnDecl
        { name = "renderStaticText"
        , params = []
        , returnType = F80.Type.Unit
        , body =
            [ CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 5
                    , Int 5
                    , String "Counter: "
                    ]
                }
            , CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 5
                    , Int 7
                    , String "Press J (-) or K (+)"
                    ]
                }
            ]
        }
    , FnDecl
        { name = "renderCounter"
        , params = [ Param "counter" F80.Type.U8 ]
        , returnType = F80.Type.Unit
        , body =
            [ CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 14
                    , Int 5
                    , String "   "
                    ]
                }
            , DefineConst
                { name = "counterStr"
                , value =
                    CallExpr
                        { fn = "String.fromU8"
                        , args = [ Var "counter" ]
                        }
                }
            , CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 14
                    , Int 5
                    , Var "counterStr"
                    ]
                }
            ]
        }
    , FnDecl
        { name = "main"
        , params = []
        , returnType = F80.Type.Unit
        , body =
            [ CallStmt
                { fn = "ROM.clearScreen"
                , args = []
                }
            , DefineLet
                { name = "counter"
                , value = Int 0
                }
            , CallStmt
                { fn = "renderStaticText"
                , args = []
                }
            , Loop
                [ CallStmt
                    { fn = "renderCounter"
                    , args = [ Var "counter" ]
                    }
                , WaitForKeypress
                    [ { on = KeyPattern_J
                      , body =
                            [ Assign
                                { var = "counter"
                                , op = Just BOp_Sub
                                , value = Int 1
                                }
                            ]
                      }
                    , { on = KeyPattern_K
                      , body =
                            [ Assign
                                { var = "counter"
                                , op = Just BOp_Add
                                , value = Int 1
                                }
                            ]
                      }
                    ]
                ]
            ]
        }
    ]


assembly : String
assembly =
    """
AT EQU 0x16
ROM_CLS EQU 0x0daf
_string_0_0_length EQU 9
_string_0_1_length EQU 20
_string_1_0_length EQU 3
org 0x8000
main:
    call ROM_CLS
    ld a,0
    push af
    call renderStaticText
decl_main_stmt_3:
    ld ix,2
    add ix,sp
    ld a,(ix-1)
    push af
    call renderCounter
    ld ix,2
    add ix,sp
    ld sp,ix
    jp _wait_decl_main_stmt_3_loop_stmt_1_start
_wait_decl_main_stmt_3_loop_stmt_1_end:
    jp decl_main_stmt_3
_end:
    jp _end
_renderText:
    ld a,AT
    rst 0x10
    ld a,e
    rst 0x10
    ld a,d
    rst 0x10
_renderTextLoop:
    ld a,(hl)
    cp 0
    ret z
    rst 0x10
    inc hl
    jr _renderTextLoop
_stringFromU8:
    ld hl,_stringFromU8Buffer+3
    ld (hl),0
    dec hl
    ld d,0
    ld b,10
_stringFromU8Loop:
    call _u8DivMod
    add a,0x30
    ld (hl),a
    dec hl
    inc d
    ld a,c
    or a
    jr nz,_stringFromU8Loop
    inc hl
    ret
_u8DivMod:
    ld c,0
_u8DivModLoop:
    sub b
    jr c,_u8DivModEnd
    inc c
    jr _u8DivModLoop
_u8DivModEnd:
    add a,b
    ret
_wait_decl_main_stmt_3_loop_stmt_1_start:
_wait_decl_main_stmt_3_loop_stmt_1_none_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_3_loop_stmt_1_none_pressed
    bit 2,a
    jp z,_wait_decl_main_stmt_3_loop_stmt_1_none_pressed
_wait_decl_main_stmt_3_loop_stmt_1_any_pressed:
    ld a,0xbf
    in a,(0xfe)
    bit 3,a
    jp z,_wait_decl_main_stmt_3_loop_stmt_1_onJ
    bit 2,a
    jp z,_wait_decl_main_stmt_3_loop_stmt_1_onK
    jp _wait_decl_main_stmt_3_loop_stmt_1_any_pressed
_wait_decl_main_stmt_3_loop_stmt_1_onJ:
    ld a,1
    ld b,a
    ld ix,2
    add ix,sp
    ld a,(ix-1)
    sub b
    ld (ix-1),a
    jp _wait_decl_main_stmt_3_loop_stmt_1_end
_wait_decl_main_stmt_3_loop_stmt_1_onK:
    ld a,1
    ld b,a
    ld ix,2
    add ix,sp
    ld a,(ix-1)
    add a,b
    ld (ix-1),a
    jp _wait_decl_main_stmt_3_loop_stmt_1_end
renderCounter:
    ld de,3589
    ld hl,_string_1_0
    call _renderText
    ld ix,4
    add ix,sp
    ld a,(ix-1)
    call _stringFromU8
    push hl
    ld de,3589
    ld ix,6
    add ix,sp
    ld h,(ix-5)
    ld l,(ix-6)
    call _renderText
    ld ix,2
    add ix,sp
    ld sp,ix
    ret
renderStaticText:
    ld de,1285
    ld hl,_string_0_0
    call _renderText
    ld de,1287
    ld hl,_string_0_1
    call _renderText
    ret
_stringFromU8Buffer db 4
_string_0_0 db 'Counter: ', 0
_string_0_1 db 'Press J (-) or K (+)', 0
_string_1_0 db '   ', 0
end 0x8000
    """
