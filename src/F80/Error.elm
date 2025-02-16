module F80.Error exposing (Error(..))

import F80.Emitter
import F80.Parser


type Error
    = ParserError F80.Parser.Error
    | EmitterError F80.Emitter.Error
