module F80.Lower.AddImplicitReturns exposing (add)

import F80.AST exposing (Decl(..), Expr(..), Program, Stmt(..))
import F80.Emitter.Util as Util
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
                        if fn.name == Util.mainFnName then
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
