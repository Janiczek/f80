port module F80 exposing (main)

import Example
import F80.Emitter
import F80.Error
import F80.Lower.HoistStringLiterals
import F80.Parser
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
        |> Result.andThen (F80.Lower.HoistStringLiterals.hoistStringLiterals >> Result.mapError F80.Error.HoistStringLiteralsError)
        |> Debug.log "lowered (hoisted string literals to globals)"
        -- TODO: F80.TypeInference.infer
        |> Result.andThen (F80.Emitter.emit >> Result.mapError F80.Error.EmitterError)
        |> Debug.log "emitted"


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
