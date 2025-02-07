module F80.Error exposing (Error(..))

import F80.Parser
import F80.Z80Asm


type Error
    = ParserError F80.Parser.Error
    | AsmError F80.Z80Asm.Error
