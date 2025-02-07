module F80.Z80Asm exposing (Error, assemble)

import F80.AST


type Error
    = TodoAsmError


assemble : F80.AST.Program -> Result Error ()
assemble program =
    Debug.todo "F80.Z80Asm.assemble"
