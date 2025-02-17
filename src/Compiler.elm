port module Compiler exposing (main)

{-| The F80 language compiler.

String
--> Parse.parse
AST.Program
--> SSA.translate
SSA.Program
--> RegAlloc.allocate
RegAlloc.Program
--> Emit.emit
Asm.Program
--> Asm.toString
String

-}

import Emit
import Error
import Example
import Parse
import RegAlloc
import SSA


port println : String -> Cmd msg


main : Program () () Never
main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


run : String -> Result Error.Error ()
run sourceCode =
    Ok sourceCode
        |> Debug.log "Source code"
        |> Result.andThen (Parse.parse >> Result.mapError Error.ParseError)
        |> Debug.log "Parsed AST"
        -- TODO: TypeInference.infer
        |> Result.andThen (SSA.translate >> Result.mapError Error.SSAError)
        |> Debug.log "SSA"
        |> Result.andThen (RegAlloc.allocate >> Result.mapError Error.RegAllocError)
        |> Debug.log "RegAlloc"
        |> Result.andThen (Emit.emit >> Result.mapError Error.EmitError)
        |> Debug.log "Emit Asm"


init : () -> ( (), Cmd Never )
init () =
    let
        result : Result Error.Error ()
        result =
            run Example.sourceCode
    in
    ( ()
    , println (Debug.toString result)
    )
