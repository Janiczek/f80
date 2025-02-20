module F80.Emitter.WaitForKeypress exposing (emit)

import Dict
import F80.AST exposing (KeyPattern(..), Stmt, WaitForKeypressItem)
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.Util as Util exposing (i, l)
import Set


emit :
    List String
    -> (List String -> List Stmt -> Output)
    -> List WaitForKeypressItem
    -> Output
emit ctx emitBlock cases =
    case cases of
        [] ->
            Debug.todo "Define what should happen when the wait has no patterns. 'Press any key'?"

        _ ->
            emit_ ctx emitBlock cases


emit_ :
    List String
    -> (List String -> List Stmt -> Output)
    -> List WaitForKeypressItem
    -> Output
emit_ ctx emitBlock cases =
    let
        ctxLabel =
            Util.ctxLabel ctx

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

        ( auxOutput, caseBlocks ) =
            List.foldl
                (\case_ ( accOutput, accBlocks ) ->
                    let
                        keyLabel_ =
                            keyLabel onPrefix case_.on

                        blockOutput : Output
                        blockOutput =
                            emitBlock (keyLabel_ :: ctx) case_.body

                        blockOutputWithoutCode =
                            { blockOutput | mainCode = [] }
                    in
                    ( accOutput
                        |> Output.add blockOutputWithoutCode
                    , List.concat
                        [ [ l keyLabel_ ]
                        , blockOutput.mainCode
                        , [ i <| "jp " ++ endLabel ]
                        ]
                        :: accBlocks
                    )
                )
                ( Output.empty, [] )
                cases
    in
    { equs = Dict.empty
    , mainCode =
        [ i <| "jp " ++ startLabel
        , l endLabel
        ]
    , otherBlocks =
        [ [ l startLabel
          , l nonePressedLabel

          {- TODO later when we have more keys to pattern match on: group
             these reads. Right now we only have J and K which are luckily
             on a single port.
          -}
          , i "ld a,0xbf" -- the group with H,J,K,L,Enter
          , i "in a,(0xfe)"
          ]
        , cases
            |> List.concatMap
                (\case_ ->
                    [ i <| "bit " ++ String.fromInt (bit case_.on) ++ ",a"
                    , i <| "jp z," ++ nonePressedLabel
                    ]
                )
        , [ l anyPressedLabel

          {- TODO later when we have more keys to pattern match on: group
             these reads. Right now we only have J and K which are luckily
             on a single port.
          -}
          , i "ld a,0xbf" -- the group with H,J,K,L,Enter
          , i "in a,(0xfe)"
          ]
        , cases
            |> List.concatMap
                (\case_ ->
                    [ i <| "bit " ++ String.fromInt (bit case_.on) ++ ",a"
                    , i <| "jp z," ++ keyLabel onPrefix case_.on
                    ]
                )
        , [ i <| "jp " ++ anyPressedLabel ]
        , caseBlocks
            |> List.reverse
            |> List.concat
        ]
            |> List.concat
            |> Set.singleton
    , data = []
    }
        |> Output.add auxOutput


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
