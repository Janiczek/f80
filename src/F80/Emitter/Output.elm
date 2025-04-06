module F80.Emitter.Output exposing
    ( Output
    , empty, smush, add, toString, fromList
    , andThen
    , db, other, code, equ
    , renderText, romCls, stringFromU8, u8DivMod
    )

{-|

@docs Output
@docs empty, smush, add, toString, fromList
@docs andThen
@docs db, other, code, equ


## Standard library

@docs renderText, romCls, stringFromU8, u8DivMod

-}

import Dict exposing (Dict)
import F80.Emitter.Util exposing (i, l)
import Set exposing (Set)


type alias Output =
    { equs : Dict String String
    , -- This is special: inside emitStmt this will contain the currently
      -- emitted code and needs to be andThen'd properly. So it doesn't always
      -- mean `main()`.
      mainCode : List String
    , otherBlocks : Dict String (List String)
    , data : Dict String String
    }


toString : Output -> List String
toString output =
    List.concat
        [ output.equs
            |> Dict.toList
            |> List.map (\( k, v ) -> k ++ " EQU " ++ v)
        , F80.Emitter.Util.mainPrologue
        , output.mainCode
        , F80.Emitter.Util.mainEpilogue
        , List.concat (Dict.values output.otherBlocks)
        , output.data
            |> Dict.toList
            |> List.map (\( k, v ) -> k ++ " db " ++ v)
        , F80.Emitter.Util.programEpilogue
        ]


smush : Output -> Output -> Output
smush s1 s2 =
    { equs = Dict.union s1.equs s2.equs
    , mainCode = s1.mainCode ++ s2.mainCode
    , otherBlocks = Dict.union s1.otherBlocks s2.otherBlocks
    , data = Dict.union s1.data s2.data
    }


add : Output -> Output -> Output
add s2 s1 =
    smush s1 s2


empty : Output
empty =
    { equs = Dict.empty
    , mainCode = []
    , otherBlocks = Dict.empty
    , data = Dict.empty
    }


db : String -> String -> Output
db name value =
    { empty | data = Dict.singleton name value }


other : String -> List String -> Output
other name asm =
    { empty | otherBlocks = Dict.singleton name asm }


code : List String -> Output
code asm =
    { empty | mainCode = asm }


equ : String -> String -> Output
equ name value =
    { empty | equs = Dict.singleton name value }


{-| Render.text(x: U8, y: U8, s: String)

Expects x,y in DE, null-terminated string address in HL.
Doesn't return anything.

-}
renderText : Output
renderText =
    { equs = Dict.singleton "AT" "0x16"
    , mainCode = []
    , otherBlocks =
        Dict.fromList
            [ ( "_renderText"
              , [ l "_renderText"
                , i "ld a,AT"
                , i "rst 0x10"
                , i "ld a,e"
                , i "rst 0x10"
                , i "ld a,d"
                , i "rst 0x10"
                , l "_renderTextLoop"
                , i "ld a,(hl)"
                , i "cp 0"
                , i "ret z"
                , i "rst 0x10"
                , i "inc hl"
                , i "jr _renderTextLoop"
                ]
              )
            ]
    , data = Dict.empty
    }


{-| String.fromU8(n: U8): String

Expects the number in A.
Returns null-terminated string address in HL

-}
stringFromU8 : Output
stringFromU8 =
    { equs = Dict.empty
    , mainCode = []
    , otherBlocks =
        Dict.fromList
            [ ( "_stringFromU8"
              , [ l "_stringFromU8"
                , i "ld hl,_stringFromU8Buffer+3"
                , i "ld (hl),0"
                , i "dec hl"
                , i "ld d,0"
                , i "ld b,10"
                , l "_stringFromU8Loop"
                , i "call _u8DivMod"
                , i "add a,0x30"
                , i "ld (hl),a"
                , i "dec hl"
                , i "inc d"
                , i "ld a,c"
                , i "or a"
                , i "jr nz,_stringFromU8Loop"
                , i "inc hl"
                , i "ret"
                ]
              )
            ]
    , data = Dict.singleton "_stringFromU8Buffer" "4" -- 3 digits + null
    }
        |> add u8DivMod


{-| U8.divMod(n: U8, d: U8): (quot: U8, rem: U8)

Expects the dividend in A, the divisor in B.
Returns quotient in A, remainder in C.

-}
u8DivMod : Output
u8DivMod =
    { equs = Dict.empty
    , mainCode = []
    , otherBlocks =
        Dict.fromList
            [ ( "_u8DivMod"
              , [ l "_u8DivMod"
                , i "ld c,0"
                , l "_u8DivModLoop"
                , i "sub b"
                , i "jr c,_u8DivModEnd"
                , i "inc c"
                , i "jr _u8DivModLoop"
                , l "_u8DivModEnd"
                , i "add a,b"
                , i "ret"
                ]
              )
            ]
    , data = Dict.empty
    }


romCls : Output
romCls =
    { equs = Dict.singleton "ROM_CLS" "0x0daf"
    , mainCode = [ i "call ROM_CLS" ]
    , otherBlocks = Dict.empty
    , data = Dict.empty
    }


fromList : List Output -> Output
fromList os =
    List.foldl add empty os


andThen : (List String -> Output) -> Output -> Output
andThen f o1 =
    empty
        |> add { o1 | mainCode = [] }
        |> add (f o1.mainCode)
