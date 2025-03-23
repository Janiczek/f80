module F80.Emitter.Output exposing
    ( Output
    , empty, smush, add, toString, fromList
    , andThen
    , db, other, code, equ
    , renderText, romCls
    )

{-|

@docs Output
@docs empty, smush, add, toString, fromList
@docs andThen
@docs db, other, code, equ


## Standard library

@docs renderText, romCls

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

Expects x,y in HL, null-terminated string address in DE.
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
                , i "ld a,l"
                , i "rst 0x10"
                , i "ld a,h"
                , i "rst 0x10"
                , l "_renderTextLoop"
                , i "ld a,(de)"
                , i "cp 0"
                , i "ret z"
                , i "rst 0x10"
                , i "inc de"
                , i "jr _renderTextLoop"
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
