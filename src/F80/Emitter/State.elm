module F80.Emitter.State exposing
    ( State, empty, mapState, initWith, forEach
    , getVar, getCurrentFrameSize
    , lift_SS_SOS, lift_SOS_OSOS, lift_SS_OSOS
    , withContext
    , withFrame
    , addLocalVar
    , addGlobalVar
    , i, l, equ, add
    , emit, otherBlock
    )

{-|

@docs State, empty, mapState, initWith, forEach
@docs getVar, getCurrentFrameSize
@docs lift_SS_SOS, lift_SOS_OSOS, lift_SS_OSOS


# Context (for labels)

@docs withContext


# Frames (functions, blocks)

@docs withFrame


# Local vars

@docs addLocalVar


# Global vars

@docs addGlobalVar


# Assembly instructions

@docs push, pop, i, l, equ, add
@docs emit, otherBlock

-}

import Dict exposing (Dict)
import F80.Emitter.Output as Output exposing (Output)
import F80.Emitter.Util as Util exposing (i)
import NonemptyList exposing (NonemptyList)
import Set exposing (Set)


type alias State =
    { ctx : List String
    , stackFrames : NonemptyList Frame
    , globalVars : Set String
    }


type alias Frame =
    { id : String -- not strictly necessary, but helpful for debugging
    , locals : Dict String Int -- local vars for this stack frame, with the offset from the base pointer
    }


{-| 2 because we need to push `af` but we only use `a`
-}
stackItemSize : Int
stackItemSize =
    2


empty : State
empty =
    { ctx = []
    , stackFrames =
        NonemptyList.singleton
            { id = "__root"
            , locals = Dict.empty
            }
    , globalVars = Set.empty
    }


mapState : (State -> State) -> ( Output, State ) -> ( Output, State )
mapState fn ( output, state ) =
    ( output, fn state )


lift_SS_SOS : (State -> State) -> (State -> ( Output, State ))
lift_SS_SOS fn s =
    let
        s2 =
            fn s
    in
    ( Output.empty, s2 )


lift_SOS_OSOS : (State -> ( Output, State )) -> (( Output, State ) -> ( Output, State ))
lift_SOS_OSOS fn ( o, s ) =
    let
        ( o2, s2 ) =
            fn s
    in
    ( Output.smush o o2, s2 )


lift_SS_OSOS : (State -> State) -> (( Output, State ) -> ( Output, State ))
lift_SS_OSOS fn =
    fn
        |> lift_SS_SOS
        |> lift_SOS_OSOS


initWith : State -> ( Output, State )
initWith state =
    ( Output.empty, state )


forEach : List a -> (a -> State -> ( Output, State )) -> ( Output, State ) -> ( Output, State )
forEach list fn os =
    List.foldl
        (\item ( o, s ) ->
            let
                ( o2, s2 ) =
                    fn item s
            in
            ( Output.smush o o2, s2 )
        )
        os
        list



-- Context


pushContext : String -> State -> State
pushContext addition state =
    { state | ctx = addition :: state.ctx }


popContext : State -> State
popContext state =
    { state | ctx = List.drop 1 state.ctx }


withContext : String -> (State -> ( Output, State )) -> ( Output, State ) -> ( Output, State )
withContext contextName fn ( output, state ) =
    let
        ( output2, state2 ) =
            state
                |> pushContext contextName
                |> fn
    in
    ( Output.smush output output2
    , state2 |> popContext
    )



-- Frames


withFrame : String -> (State -> ( Output, State )) -> ( Output, State ) -> ( Output, State )
withFrame id fn ( o1, s1 ) =
    let
        s2 =
            startFrame id s1

        ( o3, s3 ) =
            fn s2
    in
    ( Output.smush o1 o3
    , s3 |> endFrame
    )


startFrame : String -> State -> State
startFrame id state =
    { state
        | stackFrames =
            state.stackFrames
                |> NonemptyList.cons { id = id, locals = Dict.empty }
    }


endFrame : State -> State
endFrame state =
    { state
        | stackFrames =
            case NonemptyList.tail state.stackFrames of
                Nothing ->
                    Debug.todo "BUG: endFrame: somehow we've dropped the root frame that nobody started. This means we have an unbalanced count of startFrame and endFrame calls"

                Just rest ->
                    rest
    }



-- Local vars


{-| There's no removeLocalVar because they'll be automatically dropped at the end of the current frame
This assumes you've already pushed this onto the stack:

    |> State.i "push af"
    |> State.addLocalVar "a"

-}
addLocalVar : String -> State -> State
addLocalVar name state =
    { state
        | stackFrames =
            state.stackFrames
                |> NonemptyList.mapHead
                    (\frame ->
                        { frame
                            | locals =
                                Dict.update name
                                    (\maybeStack ->
                                        case maybeStack of
                                            Nothing ->
                                                let
                                                    stackOffset =
                                                        getCurrentFrameSize_ frame + 1
                                                in
                                                Just stackOffset

                                            Just old ->
                                                "BUG: shadowing of local {VAR} in frame {FRAME} (context: {CTX}). Should have been caught by the type checker"
                                                    |> String.replace "{VAR}" name
                                                    |> String.replace "{FRAME}" frame.id
                                                    |> String.replace "{CTX}" (String.join "." state.ctx)
                                                    |> Debug.todo
                                    )
                                    frame.locals
                        }
                    )
    }



-- Monadic interface


emit : (State -> ( Output, State )) -> ( Output, State ) -> ( Output, State )
emit =
    lift_SOS_OSOS


i : String -> ( Output, State ) -> ( Output, State )
i name ( output, state ) =
    ( output |> Output.add (Output.code [ Util.i name ])
    , state
    )


l : String -> ( Output, State ) -> ( Output, State )
l name ( output, state ) =
    ( output |> Output.add (Output.code [ Util.l name ])
    , state
    )


addGlobalVar : String -> String -> ( Output, State ) -> ( Output, State )
addGlobalVar name value ( output, state ) =
    if Set.member name state.globalVars then
        Debug.todo <| "Global variable already exists: " ++ name

    else
        ( output |> Output.add (Output.db name value)
        , { state | globalVars = Set.insert name state.globalVars }
        )


equ : String -> String -> ( Output, State ) -> ( Output, State )
equ name value ( output, state ) =
    ( output |> Output.add (Output.equ name value)
    , state
    )


add : Output -> ( Output, State ) -> ( Output, State )
add output2 ( output, state ) =
    ( Output.smush output output2, state )


otherBlock : String -> (State -> ( Output, State )) -> ( Output, State ) -> ( Output, State )
otherBlock label fn ( output, state ) =
    let
        ( output2, state2 ) =
            fn state

        output2AsOther =
            { output2
                | otherBlocks = Dict.singleton label output2.mainCode
                , mainCode = []
            }
    in
    ( Output.smush output output2AsOther
    , state2
    )


getVar : String -> State -> Maybe { stackOffset : Int }
getVar name state =
    getVarHelp name state.stackFrames


{-| A linear walk through the frames
-}
getVarHelp : String -> NonemptyList Frame -> Maybe { stackOffset : Int }
getVarHelp name ( f, fs ) =
    case Dict.get name f.locals of
        Just stackOffset ->
            Just { stackOffset = stackOffset }

        Nothing ->
            case NonemptyList.fromList fs of
                Nothing ->
                    Nothing

                Just rest ->
                    getVarHelp name rest


getCurrentFrameSize : State -> Int
getCurrentFrameSize state =
    let
        ( frame, _ ) =
            state.stackFrames
    in
    getCurrentFrameSize_ frame


getCurrentFrameSize_ : Frame -> Int
getCurrentFrameSize_ frame =
    Dict.size frame.locals * stackItemSize
