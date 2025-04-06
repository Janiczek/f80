port module F80 exposing (main)

import Example
import F80.Emitter
import F80.Error
import F80.Lower
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


run : String -> Result F80.Error.Error String
run sourceCode =
    Ok sourceCode
        |> Result.andThen (F80.Parser.parse >> Result.mapError F80.Error.ParserError)
        --|> Debug.log "parsed"
        |> Result.map F80.Lower.lower
        |> Result.andThen (F80.Emitter.emit >> Result.mapError F80.Error.TyperError)
        --|> Debug.log "emitted"
        |> Result.map (String.join "\n")


init : () -> ( (), Cmd Never )
init () =
    let
        result : Result F80.Error.Error String
        result =
            run Example.sourceCode
    in
    ( ()
    , println (Debug.toString result)
    )
