module Asm exposing
    ( Program, toString
    , Line(..)
    , Op(..)
    )

{-| Z80 Assembly

@docs Program, toString
@docs Line
@docs Op

-}


type alias Program =
    List Line


type Line
    = Label String
    | Op Op


type Op
    = LdA Int


toString : Program -> String
toString program =
    program
        |> List.map lineToString
        |> String.join "\n"


lineToString : Line -> String
lineToString line =
    case line of
        Label label ->
            label ++ ":"

        Op op ->
            "    " ++ opToString op


opToString : Op -> String
opToString op =
    case op of
        LdA n ->
            "ld a, " ++ String.fromInt n
