module F80.Lower.AddImplicitReturns exposing (add)

{-| We don't add implicit returns to the main function, because it has the _end: jp _end loop right after.
-}

import F80.AST as AST exposing (Decl(..), Expr(..), Program, Stmt(..))
import List.Extra


add : Program -> Program
add program =
    program
        |> List.map
            (\decl ->
                case decl of
                    GlobalDecl _ ->
                        decl

                    FnDecl fn ->
                        if fn.name == AST.mainFnName then
                            decl

                        else
                            let
                                withEmptyReturn () =
                                    FnDecl { fn | body = fn.body ++ [ Return Nothing ] }
                            in
                            case List.Extra.last fn.body of
                                Just (Return _) ->
                                    decl

                                Just _ ->
                                    withEmptyReturn ()

                                Nothing ->
                                    withEmptyReturn ()
            )
