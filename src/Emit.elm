module Emit exposing (emit, Error)

{-| We use the calling convention `__smallc` from Z88DK:

The parameters are pushed onto the stack from left to right.
Return values are in hl, or dehl.
The caller is responsible for cleaning the stack.

<https://github.com/z88dk/z88dk/wiki/CallingConventions>

@docs emit, Error

-}

import Asm
import RegAlloc


type Error
    = TodoEmitError


type alias State =
    { main : Asm.Program
    , fns : List Asm.Program
    , data : List Asm.Program
    }


initState : State
initState =
    { main = []
    , fns = []
    , data = []
    }


combineState : State -> State -> State
combineState state1 state2 =
    { main = state1.main ++ state2.main
    , fns = state1.fns ++ state2.fns
    , data = state1.data ++ state2.data
    }


emit : RegAlloc.Program -> Result Error Asm.Program
emit program =
    resultFoldMap (Debug.todo "emitDecl") combineState initState program
        |> Result.map toProgram


toProgram : State -> Asm.Program
toProgram state =
    List.concat
        [ state.main
        , List.concat state.fns
        , List.concat state.data
        ]


resultFoldMap :
    (input -> Result error state)
    -> (state -> state -> state)
    -> state
    -> List input
    -> Result error state
resultFoldMap singleStep combineSteps init inputs =
    case inputs of
        [] ->
            Ok init

        input :: restOfInputs ->
            case singleStep input of
                Err err ->
                    Err err

                Ok new ->
                    resultFoldMap singleStep combineSteps (combineSteps init new) restOfInputs
