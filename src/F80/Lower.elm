module F80.Lower exposing (lower)

import F80.AST
import F80.Lower.AddImplicitReturns
import F80.Lower.HoistStringLiterals


lower : F80.AST.Program -> F80.AST.Program
lower program =
    program
        |> F80.Lower.HoistStringLiterals.hoist
        |> F80.Lower.AddImplicitReturns.add
