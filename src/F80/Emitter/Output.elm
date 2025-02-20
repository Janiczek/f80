module F80.Emitter.Output exposing
    ( Output
    , empty, smush, add, toString
    , db, fn, code, equ
    , renderText
    )

{-|

@docs Output
@docs empty, smush, add, toString
@docs db, fn, code, equ
@docs renderText

-}

import Dict exposing (Dict)
import F80.Emitter.Util exposing (i, l)


type alias Output =
    { equs : Dict String String
    , mainCode : List String -- This is special: inside emitStmt this will contain the currently emitted code. So it doesn't always mean `main()`.
    , functions : List (List String)
    , data : List String
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
        , List.concat output.functions
        , output.data
        ]


smush : Output -> Output -> Output
smush s1 s2 =
    { equs = Dict.union s1.equs s2.equs
    , mainCode = s1.mainCode ++ s2.mainCode
    , functions = s1.functions ++ s2.functions
    , data = s1.data ++ s2.data
    }


add : Output -> Output -> Output
add s2 s1 =
    smush s1 s2


empty : Output
empty =
    { equs = Dict.empty
    , mainCode = []
    , functions = []
    , data = []
    }


db : String -> String -> Output
db name value =
    { empty | data = [ name ++ " db " ++ value ] }


fn : List String -> Output
fn asm =
    { empty | functions = [ asm ] }


code : List String -> Output
code asm =
    { empty | mainCode = asm }


equ : String -> String -> Output
equ name value =
    { empty | equs = Dict.singleton name value }


{-| Expects x,y in HL, null-terminated string address in DE
-}
renderText : Output
renderText =
    { equs = Dict.singleton "AT" "0x16"
    , mainCode = []
    , functions =
        [ [ l "renderString"
          , i "ld a, AT"
          , i "rst 0x10"
          , i "ld a,l"
          , i "rst 0x10"
          , i "ld a,h"
          , i "rst 0x10"

          --
          , l "renderStringLoop"
          , i "ld a,(de)"
          , i "cp 0"
          , i "ret z"
          , i "rst 0x10"
          , i "inc de"
          , i "jr renderStringLoop"
          ]
        ]
    , data = []
    }
