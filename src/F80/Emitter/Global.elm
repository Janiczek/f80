module F80.Emitter.Global exposing (emitGlobalDecl)

import F80.AST exposing (GlobalDeclData, Value(..))
import F80.Emitter.Util


emitGlobalDecl : GlobalDeclData -> List String
emitGlobalDecl globalData =
    let
        default () =
            [ globalData.name ++ " db " ++ emitValue globalData.value ]
    in
    case globalData.value of
        VInt _ ->
            default ()

        VBytes _ ->
            default ()

        VBinOp _ ->
            default ()

        VStringLength _ ->
            default ()

        VString s ->
            -- Also make a label for the end of the string so that we can compute the length
            [ globalData.name ++ " db " ++ emitValue globalData.value
            , F80.Emitter.Util.globalStringLengthLabel globalData.name ++ " EQU " ++ String.fromInt (String.length s)
            ]

        VGlobal otherName ->
            -- Use EQU so that we don't allocate the same data multiple times
            [ globalData.name ++ " EQU " ++ otherName ]


emitValue : Value -> String
emitValue val =
    case val of
        VInt i ->
            String.fromInt i

        VString s ->
            "'" ++ s ++ "', 0"

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

        VStringLength val_ ->
            case val_ of
                VString s ->
                    emitValue (VInt (String.length s))

                VGlobal otherName ->
                    F80.Emitter.Util.globalStringLengthLabel otherName

                _ ->
                    Debug.todo "String length used on a non-string/global value - should have been caught by the typechecker"
