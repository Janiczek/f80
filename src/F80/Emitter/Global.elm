module F80.Emitter.Global exposing (emit)

import F80.AST exposing (GlobalDeclData, Value(..))
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.State as State exposing (State)
import F80.Emitter.Util


emit : GlobalDeclData -> State -> ( Output, State )
emit globalData state =
    let
        default () =
            State.initWith state
                |> State.addGlobalVar globalData.name (emitValue globalData.value)
    in
    case globalData.value of
        VInt _ ->
            default ()

        VBytes _ ->
            default ()

        VBinOp _ ->
            default ()

        VUnaryOp _ ->
            default ()

        VBool _ ->
            default ()

        VStringLength _ ->
            default ()

        VString s ->
            State.initWith state
                |> State.addGlobalVar globalData.name (emitValue globalData.value)
                |> -- Add EQU for the string length
                   State.equ
                    (F80.Emitter.Util.globalStringLengthLabel globalData.name)
                    (String.fromInt (String.length s))

        VGlobal otherName ->
            -- Use EQU so that we don't allocate the same data multiple times
            State.initWith state
                |> State.equ globalData.name otherName


emitValue : Value -> String
emitValue val =
    case val of
        VInt i ->
            String.fromInt i

        VString s ->
            "'" ++ s ++ "', 0"

        VBool bool ->
            if bool then
                "255"

            else
                "0"

        VBytes bytes ->
            bytes
                |> List.map String.fromInt
                |> String.join ", "

        VGlobal otherName ->
            otherName

        VBinOp binOpData ->
            emitValue binOpData.left
                ++ " "
                ++ F80.Emitter.Util.emitBinOp binOpData.op
                ++ " "
                ++ emitValue binOpData.right

        VUnaryOp unaryOpData ->
            F80.Emitter.Util.emitUnaryOp unaryOpData.op
                ++ " "
                ++ emitValue unaryOpData.value

        VStringLength val_ ->
            case val_ of
                VString s ->
                    emitValue (VInt (String.length s))

                VGlobal otherName ->
                    F80.Emitter.Util.globalStringLengthLabel otherName

                _ ->
                    Debug.todo "String length used on a non-string/global value - should have been caught by the typechecker"
