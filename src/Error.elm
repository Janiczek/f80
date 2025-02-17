module Error exposing (Error(..))

import Emit
import Parse
import RegAlloc
import SSA


type Error
    = ParseError Parse.Error
    | SSAError SSA.Error
    | RegAllocError RegAlloc.Error
    | EmitError Emit.Error
