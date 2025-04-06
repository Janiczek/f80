module F80.Emitter.WaitForKeypress exposing (emit)

import Dict
import F80.AST exposing (KeyPattern(..), Stmt, WaitForKeypressItem)
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.State as State exposing (State)
import F80.Emitter.Util as Util exposing (i, l)
import F80.Path exposing (Step(..))
import Set


emit :
    (String -> List Stmt -> State -> ( Output, State ))
    -> List WaitForKeypressItem
    -> State
    -> ( Output, State )
emit emitBlock cases state =
    case cases of
        [] ->
            Debug.todo "Define what should happen when the wait has no patterns. 'Press any key'?"

        _ ->
            emit_ emitBlock cases state


emit_ :
    (String -> List Stmt -> State -> ( Output, State ))
    -> List WaitForKeypressItem
    -> State
    -> ( Output, State )
emit_ emitBlock cases state =
    let
        ctxLabel =
            F80.Path.toLabel state.path

        waitPrefix =
            "_wait_" ++ ctxLabel ++ "_"

        startLabel =
            waitPrefix ++ "start"

        endLabel =
            waitPrefix ++ "end"

        nonePressedLabel =
            waitPrefix ++ "none_pressed"

        anyPressedLabel =
            waitPrefix ++ "any_pressed"

        onPrefix =
            waitPrefix ++ "on"

        blockIdPrefix =
            "$waitForKeypress_"

        caseBlocks =
            cases
                |> List.map
                    (\case_ ->
                        let
                            keyLabel_ =
                                keyLabel onPrefix case_.on
                        in
                        \os ->
                            os
                                |> State.l keyLabel_
                                |> State.withContext (InKeyLabel keyLabel_) (emitBlock (blockIdPrefix ++ keyLabel_) case_.body)
                                |> State.i ("jp " ++ endLabel)
                    )
    in
    State.initWith state
        |> State.i ("jp " ++ startLabel)
        |> State.l endLabel
        |> State.otherBlock startLabel
            (\state1 ->
                State.initWith state1
                    |> State.l startLabel
                    |> State.l nonePressedLabel
                    {- TODO later when we have more keys to pattern match on: group
                       these reads. Right now we only have J and K which are luckily
                       on a single port.
                    -}
                    |> State.i {- the group with H,J,K,L,Enter -} "ld a,0xbf"
                    |> State.i "in a,(0xfe)"
                    |> State.forEach cases
                        (\case_ state2 ->
                            State.initWith state2
                                |> State.i ("bit " ++ String.fromInt (bit case_.on) ++ ",a")
                                |> State.i ("jp z," ++ nonePressedLabel)
                        )
                    |> State.l anyPressedLabel
                    {- TODO later when we have more keys to pattern match on: group
                       these reads. Right now we only have J and K which are luckily
                       on a single port.
                    -}
                    |> State.i {- the group with H,J,K,L,Enter -} "ld a,0xbf"
                    |> State.i "in a,(0xfe)"
                    |> State.forEach cases
                        (\case_ state2 ->
                            State.initWith state2
                                |> State.i ("bit " ++ String.fromInt (bit case_.on) ++ ",a")
                                |> State.i ("jp z," ++ keyLabel onPrefix case_.on)
                        )
                    |> State.i ("jp " ++ anyPressedLabel)
                    |> State.forEach caseBlocks
                        (\caseBlock state2 ->
                            State.initWith state2
                                |> caseBlock
                        )
            )


keyLabel : String -> KeyPattern -> String
keyLabel prefix key =
    prefix ++ F80.AST.keyPatternName key


bit : KeyPattern -> Int
bit key =
    case key of
        KeyPattern_K ->
            2

        KeyPattern_J ->
            3
