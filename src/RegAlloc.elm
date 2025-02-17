module RegAlloc exposing (Error, Program, allocate)

{-| Poletto's "linear scan" algorithm: have a pool of free registers, assign
variables to registers based on their lifetime, pick the next free register as
needed and return them to the pool as the lifetime ends.

<https://dl.acm.org/doi/10.1145/330249.330250>

-}

import SSA


type Error
    = TodoRegAllocError


type alias Program =
    List ()


allocate : SSA.Program -> Program
allocate program =
    Debug.todo "RegAlloc.allocate"
