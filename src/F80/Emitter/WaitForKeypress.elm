module F80.Emitter.WaitForKeypress exposing (emit)

import F80.AST exposing (WaitForKeypressItem)
import F80.Emitter.Output as Output exposing (Output)


emit : List WaitForKeypressItem -> Output
emit cases =
    case cases of
        [] ->
            Debug.todo "Define what should happen when the wait has no patterns. 'Press any key'?"

        [ case_ ] ->
            emitSingle case_

        _ ->
            emitMultiple cases


emitSingle : WaitForKeypressItem -> Output
emitSingle case_ =
    Debug.todo "emitSingle"


emitMultiple : List WaitForKeypressItem -> Output
emitMultiple cases =
    Debug.todo "emitMultiple"
