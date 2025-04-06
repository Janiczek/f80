module F80.Error exposing (Error(..))

import F80.Parser
import F80.Typer


type Error
    = ParserError F80.Parser.Error
    | TyperError F80.Typer.Error
