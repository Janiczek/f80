module F80.Path exposing
    ( Path
    , Step(..)
    , toLabel
    , toString
    )


type Step
    = InDecl String
    | InStmt Int
    | InLoop
    | InParam String
    | InVBinOp
    | InVBinOpLeft
    | InVBinOpRight
    | InVUnaryOp
    | InBinOp
    | InBinOpLeft
    | InBinOpRight
    | InUnaryOp
    | InIf
    | InIfCond
    | InIfThen
    | InIfElse
    | InVar String
    | InConst String
    | InLet String
    | InKeyLabel String


type alias Path =
    List Step


toString : Path -> String
toString path =
    path
        |> List.reverse
        |> List.map stepToString
        |> String.join "_"


toLabel : Path -> String
toLabel path =
    toString path


stepToString : Step -> String
stepToString step =
    case step of
        InDecl name ->
            "decl_" ++ munge name

        InStmt ix ->
            "stmt_" ++ String.fromInt ix

        InLoop ->
            "loop"

        InParam name ->
            "param_" ++ munge name

        InVBinOp ->
            "vbinop"

        InVBinOpLeft ->
            "left"

        InVBinOpRight ->
            "right"

        InVUnaryOp ->
            "vunaryop"

        InBinOp ->
            "binop"

        InBinOpLeft ->
            "left"

        InBinOpRight ->
            "right"

        InUnaryOp ->
            "unaryop"

        InIf ->
            "if"

        InIfCond ->
            "cond"

        InIfThen ->
            "then"

        InIfElse ->
            "else"

        InConst name ->
            "const_" ++ munge name

        InLet name ->
            "let_" ++ munge name

        InVar name ->
            "var_" ++ munge name

        InKeyLabel name ->
            "key_" ++ munge name


{-| Transform the string to be safe for PASMO labels.
-}
munge : String -> String
munge str =
    str
