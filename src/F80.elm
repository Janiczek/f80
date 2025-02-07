port module F80 exposing (main)

import Example
import F80.Error
import F80.Parser
import F80.Z80Asm
import Parser


port println : String -> Cmd msg


main : Program () () Never
main =
    Platform.worker
        { init = init
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


run : String -> Result F80.Error.Error ()
run sourceCode =
    Ok sourceCode
        |> Result.andThen (F80.Parser.parse >> Result.mapError F80.Error.ParserError)
        |> Debug.log "parsed"
        -- TODO: F80.TypeInference.infer
        |> Result.andThen (F80.Z80Asm.assemble >> Result.mapError F80.Error.AsmError)
        |> Debug.log "assembled"


init : () -> ( (), Cmd Never )
init () =
    let
        result : Result F80.Error.Error ()
        result =
            run Example.sourceCode
    in
    ( ()
    , println (Debug.toString result)
    )
