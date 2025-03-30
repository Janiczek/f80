module F80.Emitter.Util exposing
    ( i, l, ctxLabel
    , mainEpilogue, mainPrologue
    , emitBinOp, emitUnaryOp
    , globalStringLengthLabel
    )

{-|

@docs i, l, ctxLabel
@docs mainEpilogue, mainPrologue
@docs emitBinOp, emitUnaryOp
@docs globalStringLengthLabel

-}

import F80.AST exposing (BinOp(..), UnaryOp(..))


l : String -> String
l label =
    label ++ ":"


i : String -> String
i instruction =
    "    " ++ instruction


ctxLabel : List String -> String
ctxLabel ctx =
    ctx
        |> List.reverse
        |> String.join "_"


mainPrologue : List String
mainPrologue =
    [ "org 0x8000"
    ]


mainEpilogue : List String
mainEpilogue =
    [ l "_end"
    , i "jp _end"
    ]


globalStringLengthLabel : String -> String
globalStringLengthLabel name =
    name ++ "_length"


emitBinOp : BinOp -> String
emitBinOp op =
    case op of
        BOp_Add ->
            "+"

        BOp_Sub ->
            "-"

        BOp_Lt ->
            "<"

        BOp_Gt ->
            ">"


emitUnaryOp : UnaryOp -> String
emitUnaryOp op =
    case op of
        UOp_Not ->
            "NOT"
