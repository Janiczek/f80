module F80.Type exposing (Type(..))


type Type
    = U8
    | String
    | Bool
    | Bytes
      -- These are not user reachable
    | Function Type Type -- no anonymous functions
    | Unit -- we say a fn doesn't return by omitting the type annotation
