module F80.Parser exposing (Error, parse)

import Char
import F80.AST exposing (BinOp(..), Decl, Expr(..), UnaryOp(..), Value(..))
import F80.Type
import Parser exposing ((|.), (|=), Parser)
import Pratt
import Set exposing (Set)


type alias Error =
    List Parser.DeadEnd


parse : String -> Result Error F80.AST.Program
parse sourceCode =
    Parser.run program sourceCode


program : Parser F80.AST.Program
program =
    Parser.succeed identity
        |. spacesAndNewlines
        |= wsList spacesAndNewlines decl


decl : Parser F80.AST.Decl
decl =
    Parser.oneOf
        [ globalDecl
        , fnDecl
        ]
        |> atLineBeginning


globalDecl : Parser F80.AST.Decl
globalDecl =
    Parser.succeed F80.AST.GlobalDeclData
        |. Parser.symbol "const"
        |. spacesOnly
        |= identifier
        |. spacesOnly
        |. Parser.symbol "="
        |. spacesOnly
        |= value
        |> Parser.map F80.AST.GlobalDecl


fnDecl : Parser F80.AST.Decl
fnDecl =
    Parser.succeed F80.AST.FnDeclData
        |= identifier
        |= paramList
        |. spacesOnly
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol ":"
                |. spacesOnly
                |= type_
            , Parser.succeed F80.Type.Unit
            ]
        |. spacesOnly
        |= block
        |> Parser.map F80.AST.FnDecl


paramList : Parser (List F80.AST.Param)
paramList =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = spacesAndNewlines
        , item = param
        , trailing = Parser.Optional
        }


argList : Parser (List Expr)
argList =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = spacesAndNewlines
        , item = expr
        , trailing = Parser.Optional
        }


param : Parser F80.AST.Param
param =
    Parser.succeed F80.AST.Param
        |= identifier
        |. spacesOnly
        |. Parser.symbol ":"
        |. spacesOnly
        |= type_


type_ : Parser F80.Type.Type
type_ =
    -- We don't want to parse function (arrow) types because we have no anonymous functions!
    Parser.oneOf
        [ Parser.succeed F80.Type.U8 |. Parser.symbol "U8"
        , Parser.succeed F80.Type.String |. Parser.symbol "String"
        , Parser.succeed F80.Type.Bool |. Parser.symbol "Bool"
        , Parser.succeed F80.Type.Bytes |. Parser.symbol "Bytes"
        ]


block : Parser F80.AST.Block
block =
    Parser.succeed identity
        |. Parser.symbol "{"
        |. spacesAndNewlines
        |= wsList spacesAndNewlines stmt
        |. spacesAndNewlines
        |. Parser.symbol "}"


stmt : Parser F80.AST.Stmt
stmt =
    Parser.succeed identity
        |. spacesOnly
        |= Parser.oneOf
            [ waitForKeypressStmt
            , returnStmt
            , loopStmt
            , ifStmt
            , defineConstStmt
            , defineLetStmt
            , identifier
                |> Parser.andThen
                    (\id ->
                        Parser.oneOf
                            [ assignStmt id -- foo += 1
                            , callStmt id -- foo(123)
                            ]
                    )
            ]


waitForKeypressStmt : Parser F80.AST.Stmt
waitForKeypressStmt =
    Parser.succeed F80.AST.WaitForKeypress
        |. Parser.symbol "wait for keypress"
        |. spacesAndNewlines
        |. Parser.symbol "{"
        |. spacesAndNewlines
        |= wsList spacesAndNewlines waitForKeypressItem
        |. spacesAndNewlines
        |. Parser.symbol "}"


waitForKeypressItem : Parser F80.AST.WaitForKeypressItem
waitForKeypressItem =
    Parser.succeed F80.AST.WaitForKeypressItem
        |. spacesOnly
        |= keyPattern
        |. spacesOnly
        |. Parser.symbol "->"
        |. spacesOnly
        |= Parser.lazy (\() -> block)


keyPattern : Parser F80.AST.KeyPattern
keyPattern =
    Parser.oneOf
        [ Parser.succeed F80.AST.KeyPattern_J |. Parser.symbol "Key.J"
        , Parser.succeed F80.AST.KeyPattern_K |. Parser.symbol "Key.K"
        ]


returnStmt : Parser F80.AST.Stmt
returnStmt =
    Parser.succeed F80.AST.Return
        |. Parser.symbol "return"
        |= Parser.oneOf
            [ Parser.succeed Just
                -- in `if (true) { return }` the space after return would screw us up without the backtrackable
                |. Parser.backtrackable spacesOnly
                |= Parser.lazy (\() -> expr)
            , Parser.succeed Nothing
            ]


loopStmt : Parser F80.AST.Stmt
loopStmt =
    Parser.succeed F80.AST.Loop
        |. Parser.symbol "loop"
        |. spacesOnly
        |= Parser.lazy (\() -> block)


ifStmt : Parser F80.AST.Stmt
ifStmt =
    Parser.succeed F80.AST.IfStmtData
        |. Parser.symbol "if"
        |. spacesOnly
        |. Parser.symbol "("
        |. spacesOnly
        |= expr
        |. spacesOnly
        |. Parser.symbol ")"
        |. spacesOnly
        |= Parser.lazy (\() -> block)
        |= maybe
            (Parser.succeed identity
                |. spacesOnly
                |. Parser.symbol "else"
                |. spacesOnly
                |= Parser.lazy (\() -> block)
            )
        |> Parser.map F80.AST.If


defineConstStmt : Parser F80.AST.Stmt
defineConstStmt =
    Parser.succeed F80.AST.DefineConstData
        |. Parser.symbol "const"
        |. spacesOnly
        |= identifier
        |. spacesOnly
        |. Parser.symbol "="
        |. spacesOnly
        |= expr
        |> Parser.map F80.AST.DefineConst


defineLetStmt : Parser F80.AST.Stmt
defineLetStmt =
    Parser.succeed F80.AST.DefineLetData
        |. Parser.symbol "let"
        |. spacesOnly
        |= identifier
        |. spacesOnly
        |. Parser.symbol "="
        |. spacesOnly
        |= expr
        |> Parser.map F80.AST.DefineLet


assignStmt : String -> Parser F80.AST.Stmt
assignStmt id =
    Parser.succeed (F80.AST.AssignData id)
        |. spacesOnly
        |= Parser.oneOf
            [ Parser.succeed Nothing |. Parser.symbol "="
            , Parser.succeed (Just BOp_Add) |. Parser.symbol "+="
            , Parser.succeed (Just BOp_Sub) |. Parser.symbol "-="
            ]
        |. spacesOnly
        |= expr
        |> Parser.map F80.AST.Assign


callStmt : String -> Parser F80.AST.Stmt
callStmt fnName =
    Parser.succeed (F80.AST.CallData fnName)
        |= argList
        |> Parser.map F80.AST.CallStmt


value : Parser Value
value =
    Pratt.expression
        { oneOf =
            [ parenthesizedValue
            , Pratt.literal stringLengthValue
            , Pratt.literal intValue
            , Pratt.literal stringValue
            , Pratt.literal boolValue
            , Pratt.literal bytesValue
            , Pratt.literal globalValue
            , Pratt.prefix 1 (Parser.symbol "!") (unaryOpValue UOp_Not)
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.symbol "+") (binOpValue BOp_Add)
            , Pratt.infixLeft 1 (Parser.symbol "-") (binOpValue BOp_Sub)
            , Pratt.infixLeft 1 (Parser.symbol "<") (binOpValue BOp_Lt)
            , Pratt.infixLeft 1 (Parser.symbol ">") (binOpValue BOp_Gt)
            ]
        , spaces = spacesOnly
        }


parenthesizedValue : Pratt.Config Value -> Parser Value
parenthesizedValue config =
    Parser.succeed identity
        |. Parser.symbol "("
        -- TODO newlines too?
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.symbol ")"


globalValue : Parser Value
globalValue =
    Parser.succeed F80.AST.VGlobal
        |= identifier


intValue : Parser Value
intValue =
    Parser.succeed F80.AST.VInt
        |= unsignedInt


stringValue : Parser Value
stringValue =
    Parser.succeed F80.AST.VString
        |. Parser.symbol "\""
        |= (Parser.chompWhile (\c -> c /= '"')
                |> Parser.getChompedString
           )
        |. Parser.symbol "\""


boolValue : Parser Value
boolValue =
    Parser.succeed F80.AST.VBool
        |= bool


bool : Parser Bool
bool =
    Parser.oneOf
        [ Parser.succeed True |. Parser.symbol "true"
        , Parser.succeed False |. Parser.symbol "false"
        ]


bytesValue : Parser Value
bytesValue =
    Parser.succeed F80.AST.VBytes
        |= Parser.sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spacesAndNewlines
            , item = Parser.int
            , trailing = Parser.Optional
            }


stringLengthValue : Parser Value
stringLengthValue =
    Parser.succeed F80.AST.VStringLength
        |. Parser.symbol "String.length("
        |. spacesOnly
        |= Parser.lazy (\() -> value)
        |. spacesOnly
        |. Parser.symbol ")"


binOpValue : BinOp -> Value -> Value -> Value
binOpValue op left right =
    F80.AST.VBinOp
        { op = op
        , left = left
        , right = right
        }


unaryOpValue : UnaryOp -> Value -> Value
unaryOpValue op value_ =
    F80.AST.VUnaryOp
        { op = op
        , value = value_
        }


expr : Parser Expr
expr =
    Pratt.expression
        { oneOf =
            [ parenthesizedExpr
            , ifExpr
            , Pratt.literal intExpr
            , Pratt.literal stringExpr
            , Pratt.literal boolExpr
            , Pratt.literal varOrCallExpr
            , Pratt.prefix 1 (Parser.symbol "!") (unaryOpExpr UOp_Not)
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.symbol "+") (binOpExpr BOp_Add)
            , Pratt.infixLeft 1 (Parser.symbol "-") (binOpExpr BOp_Sub)
            , Pratt.infixLeft 1 (Parser.symbol "<") (binOpExpr BOp_Lt)
            , Pratt.infixLeft 1 (Parser.symbol ">") (binOpExpr BOp_Gt)
            ]
        , spaces = spacesOnly
        }


parenthesizedExpr : Pratt.Config Expr -> Parser Expr
parenthesizedExpr config =
    Parser.succeed identity
        |. Parser.symbol "("
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.symbol ")"


varOrCallExpr : Parser Expr
varOrCallExpr =
    identifier
        |> Parser.andThen
            (\id ->
                Parser.oneOf
                    [ Parser.succeed (F80.AST.CallData id)
                        |= argList
                        |> Parser.map F80.AST.CallExpr
                    , Parser.succeed (F80.AST.Var id)
                    ]
            )


intExpr : Parser Expr
intExpr =
    Parser.succeed F80.AST.Int
        |= unsignedInt


unsignedInt : Parser Int
unsignedInt =
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                case String.toInt str of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem "Expected unsigned integer"
            )


stringExpr : Parser Expr
stringExpr =
    Parser.succeed F80.AST.String
        |. Parser.symbol "\""
        |= (Parser.chompWhile (\c -> c /= '"')
                |> Parser.getChompedString
           )
        |. Parser.symbol "\""


boolExpr : Parser Expr
boolExpr =
    Parser.succeed F80.AST.Bool
        |= bool


ifExpr : Pratt.Config Expr -> Parser Expr
ifExpr config =
    (Parser.succeed F80.AST.IfExprData
        |. Parser.symbol "if"
        |. spacesOnly
        |. Parser.symbol "("
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.symbol ")"
        |. spacesOnly
        |= Pratt.subExpression 0 config
        |. spacesOnly
        |. Parser.symbol "else"
        |. spacesOnly
        |= Pratt.subExpression 0 config
    )
        |> Parser.map F80.AST.IfExpr


binOpExpr : BinOp -> Expr -> Expr -> Expr
binOpExpr op left right =
    F80.AST.BinOp
        { op = op
        , left = left
        , right = right
        }


unaryOpExpr : UnaryOp -> Expr -> Expr
unaryOpExpr op expr_ =
    F80.AST.UnaryOp
        { op = op
        , expr = expr_
        }


binOp : Parser F80.AST.BinOp
binOp =
    Parser.oneOf
        [ Parser.symbol "+" |> Parser.map (\_ -> BOp_Add)
        , Parser.symbol "-" |> Parser.map (\_ -> BOp_Sub)
        , Parser.symbol "<" |> Parser.map (\_ -> BOp_Lt)
        , Parser.symbol ">" |> Parser.map (\_ -> BOp_Gt)
        ]



-- HELPERS


reservedKeywords : Set String
reservedKeywords =
    Set.fromList [ "if", "else" ]


identifier : Parser String
identifier =
    Parser.variable
        { start = \c -> Char.isLower c || Char.isUpper c
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '.' || c == '?'
        , reserved = reservedKeywords
        }


spacesOnly : Parser ()
spacesOnly =
    Parser.chompWhile (\c -> c == ' ')


newlines : Parser ()
newlines =
    Parser.chompWhile (\c -> c == '\n')


spacesAndNewlines : Parser ()
spacesAndNewlines =
    Parser.succeed ()
        |. Parser.chompWhile (\c -> c == ' ' || c == '\n')
        |. maybe lineComment


lineComment : Parser ()
lineComment =
    Parser.lineComment "//"


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    Parser.oneOf
        [ (Parser.succeed Just |= parser)
            -- TODO get rid of backtrackable?
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


wsList : Parser () -> Parser a -> Parser (List a)
wsList wsParser itemParser =
    Parser.loop [] (wsListHelp wsParser itemParser)


wsListHelp : Parser () -> Parser a -> List a -> Parser (Parser.Step (List a) (List a))
wsListHelp wsParser itemParser revItems =
    Parser.oneOf
        [ Parser.succeed (\item -> Parser.Loop (item :: revItems))
            |= itemParser
            |. wsParser
        , wsParser
            |> Parser.getChompedString
            |> Parser.map
                (\chomped ->
                    if String.isEmpty chomped then
                        Parser.Done (List.reverse revItems)

                    else
                        Parser.Loop revItems
                )
        ]


log : String -> Parser a -> Parser a
log label parser =
    Parser.map (Debug.log label) parser


atLineBeginning : Parser a -> Parser a
atLineBeginning parser =
    Parser.getCol
        |> Parser.andThen
            (\col ->
                if col == 1 then
                    parser

                else
                    Parser.problem "Expected to be at line beginning"
            )
