module F80.Emitter exposing (Error, emit)

import F80.AST


type Error
    = TodoAsmError


emit : F80.AST.Program -> Result Error ()
emit program =
    Debug.todo "F80.Emitter.emit"
