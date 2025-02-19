module F80.Emitter.Util exposing (emitBinOp, globalStringLengthLabel, i, l)

import F80.AST exposing (BinOp(..))


l : String -> String
l label =
    label ++ ":"


i : String -> String
i instruction =
    "    " ++ instruction


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
