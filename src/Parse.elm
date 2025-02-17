module Parse exposing (Error, parse)

import AST exposing (BinOp(..), Decl, Expr(..))
import Char
import Parser exposing ((|.), (|=), Parser)
import Pratt
import Set exposing (Set)


type alias Error =
    List Parser.DeadEnd


parse : String -> Result Error AST.Program
parse sourceCode =
    Parser.run program sourceCode


program : Parser AST.Program
program =
    Parser.succeed identity
        |. spacesAndNewlines
        |= wsList spacesAndNewlines decl


decl : Parser AST.Decl
decl =
    Parser.oneOf
        [ constDecl
        , fnDecl
        ]
        |> atLineBeginning


constDecl : Parser AST.Decl
constDecl =
    (Parser.succeed AST.ConstDeclData
        |. Parser.symbol "const"
        |. spacesOnly
        |= identifier
        |. spacesOnly
        |. Parser.symbol "="
        |. spacesOnly
        |= expr
    )
        |> Parser.map AST.ConstDecl


fnDecl : Parser AST.Decl
fnDecl =
    (Parser.succeed AST.FnDeclData
        |= identifier
        |= paramList
        |. spacesOnly
        |= block
    )
        |> Parser.map AST.FnDecl


paramList : Parser (List AST.Param)
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


param : Parser AST.Param
param =
    identifier


block : Parser AST.Block
block =
    Parser.succeed identity
        |. Parser.symbol "{"
        |. spacesAndNewlines
        |= wsList spacesAndNewlines stmt
        |. spacesAndNewlines
        |. Parser.symbol "}"


stmt : Parser AST.Stmt
stmt =
    Parser.succeed identity
        |. spacesOnly
        |= Parser.oneOf
            [ waitForKeyboardStmt
            , loopStmt
            , ifStmt
            , defineConstStmt
            , defineLetStmt
            , identifier
                |> Parser.andThen
                    (\id ->
                        Parser.oneOf
                            [ assignStmt id -- foo += 1
                            , callStmt (Var id) -- foo(123)
                            ]
                    )
            , expr |> Parser.andThen callStmt -- foo(bar)(baz)
            ]


waitForKeyboardStmt : Parser AST.Stmt
waitForKeyboardStmt =
    Parser.succeed AST.WaitForKeyboard
        |. Parser.symbol "wait for keyboard"
        |. spacesAndNewlines
        |. Parser.symbol "{"
        |. spacesAndNewlines
        |= wsList spacesAndNewlines waitForKeyboardItem
        |. spacesAndNewlines
        |. Parser.symbol "}"


waitForKeyboardItem : Parser AST.WaitForKeyboardItem
waitForKeyboardItem =
    Parser.succeed AST.WaitForKeyboardItem
        |. spacesOnly
        |= keyPattern
        |. spacesOnly
        |. Parser.symbol "->"
        |. spacesOnly
        |= Parser.lazy (\() -> block)


keyPattern : Parser AST.KeyPattern
keyPattern =
    Parser.oneOf
        [ Parser.succeed AST.KeyPattern_Plus |. Parser.symbol "Key.Plus"
        , Parser.succeed AST.KeyPattern_Minus |. Parser.symbol "Key.Minus"
        ]


loopStmt : Parser AST.Stmt
loopStmt =
    Parser.succeed AST.Loop
        |. Parser.symbol "loop"
        |. spacesOnly
        |= Parser.lazy (\() -> block)


ifStmt : Parser AST.Stmt
ifStmt =
    (Parser.succeed AST.IfStmtData
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
    )
        |> Parser.map AST.If


defineConstStmt : Parser AST.Stmt
defineConstStmt =
    (Parser.succeed AST.DefineConstData
        |. Parser.symbol "const"
        |. spacesOnly
        |= identifier
        |. spacesOnly
        |. Parser.symbol "="
        |. spacesOnly
        |= expr
    )
        |> Parser.map AST.DefineConst


defineLetStmt : Parser AST.Stmt
defineLetStmt =
    (Parser.succeed AST.DefineLetData
        |. Parser.symbol "let"
        |. spacesOnly
        |= identifier
        |. spacesOnly
        |. Parser.symbol "="
        |. spacesOnly
        |= expr
    )
        |> Parser.map AST.DefineLet


assignStmt : String -> Parser AST.Stmt
assignStmt id =
    (Parser.succeed (AST.AssignData id)
        |. spacesOnly
        |= Parser.oneOf
            [ Parser.succeed Nothing |. Parser.symbol "="
            , Parser.succeed (Just BOp_Add) |. Parser.symbol "+="
            , Parser.succeed (Just BOp_Sub) |. Parser.symbol "-="
            ]
        |. spacesOnly
        |= expr
    )
        |> Parser.map AST.Assign


callStmt : Expr -> Parser AST.Stmt
callStmt expr_ =
    (Parser.succeed (AST.CallData expr_)
        |= argList
    )
        |> Parser.map AST.CallStmt


expr : Parser Expr
expr =
    Pratt.expression
        { oneOf =
            [ parenthesizedExpr
            , ifExpr
            , Pratt.literal varExpr
            , Pratt.literal intExpr
            , Pratt.literal stringExpr
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.symbol "+") (binOpExpr BOp_Add)
            , Pratt.infixLeft 1 (Parser.symbol "-") (binOpExpr BOp_Sub)
            , Pratt.infixLeft 1 (Parser.symbol "<") (binOpExpr BOp_Lt)
            , Pratt.infixLeft 1 (Parser.symbol ">") (binOpExpr BOp_Gt)
            , callExpr
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


varExpr : Parser Expr
varExpr =
    Parser.succeed AST.Var
        |= identifier


intExpr : Parser Expr
intExpr =
    Parser.succeed AST.Int
        |= Parser.int


stringExpr : Parser Expr
stringExpr =
    Parser.succeed AST.String
        |. Parser.symbol "\""
        |= (Parser.chompWhile (\c -> c /= '"')
                |> Parser.getChompedString
           )
        |. Parser.symbol "\""


callExpr : Pratt.Config Expr -> ( Int, Expr -> Parser Expr )
callExpr config =
    ( 10
    , \left ->
        Parser.succeed
            (\args ->
                AST.CallExpr
                    { fn = left
                    , args = args
                    }
            )
            |= argList
    )


ifExpr : Pratt.Config Expr -> Parser Expr
ifExpr config =
    (Parser.succeed AST.IfExprData
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
        |> Parser.map AST.IfExpr


binOpExpr : BinOp -> Expr -> Expr -> Expr
binOpExpr op left right =
    AST.BinOp
        { op = op
        , left = left
        , right = right
        }


binOp : Parser AST.BinOp
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
