module F80.Emitter.Output exposing
    ( Output
    , empty, smush, add, toString, fromList
    , andThen, andThen2
    , db, other, code, equ
    , renderText, romCls
    )

{-|

@docs Output
@docs empty, smush, add, toString, fromList
@docs andThen, andThen2
@docs db, other, code, equ

Standard library

@docs renderText, romCls

-}

import Dict exposing (Dict)
import F80.Emitter.Util exposing (i, l)
import Set exposing (Set)


type alias Output =
    { equs : Dict String String
    , mainCode : List String -- This is special: inside emitStmt this will contain the currently emitted code. So it doesn't always mean `main()`.
    , otherBlocks : Set (List String)
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
        , List.concat (Set.toList output.otherBlocks)
        , output.data
        ]


smush : Output -> Output -> Output
smush s1 s2 =
    { equs = Dict.union s1.equs s2.equs
    , mainCode = s1.mainCode ++ s2.mainCode
    , otherBlocks = Set.union s1.otherBlocks s2.otherBlocks
    , data = s1.data ++ s2.data
    }


add : Output -> Output -> Output
add s2 s1 =
    smush s1 s2


empty : Output
empty =
    { equs = Dict.empty
    , mainCode = []
    , otherBlocks = Set.empty
    , data = []
    }


db : String -> String -> Output
db name value =
    { empty | data = [ name ++ " db " ++ value ] }


other : List String -> Output
other asm =
    { empty | otherBlocks = Set.singleton asm }


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
    , otherBlocks =
        Set.singleton
            [ l "_renderString"
            , i "ld a, AT"
            , i "rst 0x10"
            , i "ld a,l"
            , i "rst 0x10"
            , i "ld a,h"
            , i "rst 0x10"

            --
            , l "_renderStringLoop"
            , i "ld a,(de)"
            , i "cp 0"
            , i "ret z"
            , i "rst 0x10"
            , i "inc de"
            , i "jr _renderStringLoop"
            ]
    , data = []
    }


romCls : Output
romCls =
    { equs = Dict.singleton "ROM_CLS" "0x0daf"
    , mainCode = [ i "call ROM_CLS" ]
    , otherBlocks = Set.empty
    , data = []
    }


fromList : List Output -> Output
fromList os =
    List.foldl add empty os


andThen : (List String -> Output) -> Output -> Output
andThen f o1 =
    empty
        |> add { o1 | mainCode = [] }
        |> add (f o1.mainCode)


andThen2 : (List String -> List String -> Output) -> Output -> Output -> Output
andThen2 f o1 o2 =
    empty
        |> add { o1 | mainCode = [] }
        |> add { o2 | mainCode = [] }
        |> add (f o1.mainCode o2.mainCode)
