module ParserTest exposing (suite)

import Example
import Expect exposing (Expectation)
import F80.AST
    exposing
        ( BinOp(..)
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Stmt(..)
        )
import F80.Parser
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "F80.Parser.parse"
        [ programTests
        , declarationTests
        , statementTests
        , expressionTests
        , commentTests
        ]


programTests : Test
programTests =
    Test.describe "Program"
        [ Test.test "parses empty program" <|
            \_ ->
                ""
                    |> F80.Parser.parse
                    |> Expect.equal (Ok [])
        , Test.test "parses multiple declarations" <|
            \_ ->
                """
main() {
    foo(42)
}

bar(x) {
    foo(x)
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ CallStmt
                                        { fn = Var "foo"
                                        , args = [ Int 42 ]
                                        }
                                    ]
                                }
                            , FnDecl
                                { name = "bar"
                                , params = [ "x" ]
                                , body =
                                    [ CallStmt
                                        { fn = Var "foo"
                                        , args = [ Var "x" ]
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses the Counter example" <|
            \() ->
                Example.sourceCode
                    |> F80.Parser.parse
                    |> Expect.equal (Ok Example.ast)
        ]


declarationTests : Test
declarationTests =
    Test.describe "Declarations"
        [ constDeclTests
        , fnDeclTests
        ]


constDeclTests : Test
constDeclTests =
    Test.describe "ConstDecl"
        [ Test.test "parses const declarations" <|
            \_ ->
                """
const x = 42
const y = "hello"
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ ConstDecl { name = "x", expr = Int 42 }
                            , ConstDecl { name = "y", expr = String "hello" }
                            ]
                        )
        ]


fnDeclTests : Test
fnDeclTests =
    Test.describe "FnDecl"
        [ Test.test "parses function declarations" <|
            \_ ->
                """
main() {
    foo(42, "hello")
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ CallStmt
                                        { fn = Var "foo"
                                        , args = [ Int 42, String "hello" ]
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses function with parameters" <|
            \_ ->
                """
foo(x, y) {
    bar(x, y)
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "foo"
                                , params = [ "x", "y" ]
                                , body =
                                    [ CallStmt
                                        { fn = Var "bar"
                                        , args = [ Var "x", Var "y" ]
                                        }
                                    ]
                                }
                            ]
                        )
        ]


statementTests : Test
statementTests =
    Test.describe "Statements"
        [ waitForKeyboardTests
        , loopTests
        , ifStmtTests
        , defineConstTests
        , defineLetTests
        , assignTests
        , callStmtTests
        ]


waitForKeyboardTests : Test
waitForKeyboardTests =
    Test.describe "WaitForKeyboard"
        [ Test.test "parses wait for keyboard" <|
            \_ ->
                """
main() {
    wait for keyboard {
        Key.Plus -> { x += 1 }
        Key.Minus -> { x -= 1 }
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ WaitForKeyboard
                                        [ { on = KeyPattern_Plus
                                          , body = [ Assign { var = "x", op = Just BOp_Add, value = Int 1 } ]
                                          }
                                        , { on = KeyPattern_Minus
                                          , body = [ Assign { var = "x", op = Just BOp_Sub, value = Int 1 } ]
                                          }
                                        ]
                                    ]
                                }
                            ]
                        )
        , Test.test "parses empty wait for keyboard with newline" <|
            \_ ->
                """
main() {
    wait for keyboard {
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ WaitForKeyboard [] ]
                                }
                            ]
                        )
        , Test.test "parses empty wait for keyboard without newline" <|
            \_ ->
                """
main() {
    wait for keyboard {}
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ WaitForKeyboard [] ]
                                }
                            ]
                        )
        , Test.test "parses wait for keyboard with if stmt in its handler" <|
            \_ ->
                """
main() {
    wait for keyboard {
      Key.Minus -> { if (counter > 0) { counter -= 1 } }
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ WaitForKeyboard
                                        [ { on = KeyPattern_Minus
                                          , body =
                                                [ If
                                                    { cond =
                                                        BinOp
                                                            { left = Var "counter"
                                                            , op = BOp_Gt
                                                            , right = Int 0
                                                            }
                                                    , then_ =
                                                        [ Assign
                                                            { var = "counter"
                                                            , op = Just BOp_Sub
                                                            , value = Int 1
                                                            }
                                                        ]
                                                    , else_ = Nothing
                                                    }
                                                ]
                                          }
                                        ]
                                    ]
                                }
                            ]
                        )
        ]


loopTests : Test
loopTests =
    Test.describe "Loop"
        [ Test.test "parses loop" <|
            \_ ->
                """
main() {
    loop {
        bar()
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ Loop [ CallStmt { fn = Var "bar", args = [] } ]
                                    ]
                                }
                            ]
                        )
        , Test.test "parses empty loop" <|
            \_ ->
                """
main() {
    loop {
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ Loop [] ]
                                }
                            ]
                        )
        ]


ifStmtTests : Test
ifStmtTests =
    Test.describe "If"
        [ Test.test "parses if statement" <|
            \_ ->
                """
main() {
    if (x > 0) {
        foo()
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ If
                                        { cond = BinOp { left = Var "x", op = BOp_Gt, right = Int 0 }
                                        , then_ = [ CallStmt { fn = Var "foo", args = [] } ]
                                        , else_ = Nothing
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses if-else statement" <|
            \_ ->
                """
main() {
    if (x > 0) {
        foo()
    } else {
        bar()
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ If
                                        { cond = BinOp { left = Var "x", op = BOp_Gt, right = Int 0 }
                                        , then_ = [ CallStmt { fn = Var "foo", args = [] } ]
                                        , else_ = Just [ CallStmt { fn = Var "bar", args = [] } ]
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses if statement with simple var condition" <|
            \_ ->
                """
main() {
    if (x) {
        foo()
    }
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ If
                                        { cond = Var "x"
                                        , then_ = [ CallStmt { fn = Var "foo", args = [] } ]
                                        , else_ = Nothing
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses if statement with empty block" <|
            \_ ->
                """
main() {
    if (x) {}
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ If
                                        { cond = Var "x"
                                        , then_ = []
                                        , else_ = Nothing
                                        }
                                    ]
                                }
                            ]
                        )
        ]


defineConstTests : Test
defineConstTests =
    Test.describe "DefineConst"
        [ Test.test "parses const definition" <|
            \_ ->
                """
main() {
    const y = "hello"
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineConst { name = "y", value = String "hello" }
                                    ]
                                }
                            ]
                        )
        ]


defineLetTests : Test
defineLetTests =
    Test.describe "DefineLet"
        [ Test.test "parses let definition" <|
            \_ ->
                """
main() {
    let x = 42
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet { name = "x", value = Int 42 }
                                    ]
                                }
                            ]
                        )
        ]


assignTests : Test
assignTests =
    Test.describe "Assign"
        [ Test.test "parses assignments" <|
            \_ ->
                """
main() {
    x = 10
    x += 1
    x -= 2
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ Assign { var = "x", op = Nothing, value = Int 10 }
                                    , Assign { var = "x", op = Just BOp_Add, value = Int 1 }
                                    , Assign { var = "x", op = Just BOp_Sub, value = Int 2 }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses assignment with function call" <|
            \_ ->
                """
main() {
    x += foo(42)
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ Assign
                                        { var = "x"
                                        , op = Just BOp_Add
                                        , value = CallExpr { fn = Var "foo", args = [ Int 42 ] }
                                        }
                                    ]
                                }
                            ]
                        )
        ]


callStmtTests : Test
callStmtTests =
    Test.describe "Call"
        [ Test.test "parses function calls" <|
            \_ ->
                """
main() {
    foo(1, 2)
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ CallStmt
                                        { fn = Var "foo"
                                        , args = [ Int 1, Int 2 ]
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses function calls with newlines in arg list" <|
            \_ ->
                """
main() {
    foo(
        1,
        2
    )
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ CallStmt
                                        { fn = Var "foo"
                                        , args = [ Int 1, Int 2 ]
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "parses function calls with empty arg list" <|
            \_ ->
                """
main() {
    foo()
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ CallStmt
                                        { fn = Var "foo"
                                        , args = []
                                        }
                                    ]
                                }
                            ]
                        )
        ]


expressionTests : Test
expressionTests =
    Test.describe "Expressions"
        [ varTests
        , intTests
        , stringTests
        , binOpTests
        , callExprTests
        , ifExprTests
        ]


varTests : Test
varTests =
    Test.describe "Var"
        [ Test.test "parses variable references" <|
            \_ ->
                "main() { let x = foo }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value = Var "foo"
                                        }
                                    ]
                                }
                            ]
                        )
        ]


intTests : Test
intTests =
    Test.describe "Int"
        [ Test.test "parses integer literals" <|
            \_ ->
                "main() { let x = 42 }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value = Int 42
                                        }
                                    ]
                                }
                            ]
                        )
        ]


stringTests : Test
stringTests =
    Test.describe "String"
        [ Test.test "parses string literals" <|
            \_ ->
                "main() { let x = \"hello\" }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value = String "hello"
                                        }
                                    ]
                                }
                            ]
                        )
        ]


binOpTests : Test
binOpTests =
    let
        testBinOp : String -> BinOp -> Test
        testBinOp op binOp =
            Test.test ("parses binary operator " ++ op) <|
                \_ ->
                    "main() { let x = 1 "
                        ++ op
                        ++ " 2 }"
                        |> F80.Parser.parse
                        |> Expect.equal
                            (Ok
                                [ FnDecl
                                    { name = "main"
                                    , params = []
                                    , body =
                                        [ DefineLet
                                            { name = "x"
                                            , value = BinOp { left = Int 1, op = binOp, right = Int 2 }
                                            }
                                        ]
                                    }
                                ]
                            )
    in
    Test.describe "BinOp" <|
        List.map (\( op, binOp ) -> testBinOp op binOp)
            [ ( "+", BOp_Add )
            , ( "-", BOp_Sub )
            , ( ">", BOp_Gt )
            , ( "<", BOp_Lt )
            ]


callExprTests : Test
callExprTests =
    Test.describe "CallExpr"
        [ Test.test "parses function calls" <|
            \_ ->
                "main() { let x = foo(1, 2) }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value =
                                            CallExpr
                                                { fn = Var "foo"
                                                , args = [ Int 1, Int 2 ]
                                                }
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "optional trailing comma" <|
            \_ ->
                "main() { let x = foo(1, 2,) }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value =
                                            CallExpr
                                                { fn = Var "foo"
                                                , args = [ Int 1, Int 2 ]
                                                }
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "newlines inside the arg list" <|
            \_ ->
                """
main() { 
    let x = foo(
        1,
        2
    ) 
}"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value =
                                            CallExpr
                                                { fn = Var "foo"
                                                , args = [ Int 1, Int 2 ]
                                                }
                                        }
                                    ]
                                }
                            ]
                        )
        , Test.test "empty arg list" <|
            \_ ->
                "main() { let x = foo() }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value =
                                            CallExpr
                                                { fn = Var "foo"
                                                , args = []
                                                }
                                        }
                                    ]
                                }
                            ]
                        )
        ]


ifExprTests : Test
ifExprTests =
    Test.describe "IfExpr"
        [ Test.test "parses if expressions" <|
            \_ ->
                "main() { let x = if (a > b) 1 else 2 }"
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ DefineLet
                                        { name = "x"
                                        , value =
                                            IfExpr
                                                { cond = BinOp { left = Var "a", op = BOp_Gt, right = Var "b" }
                                                , then_ = Int 1
                                                , else_ = Int 2
                                                }
                                        }
                                    ]
                                }
                            ]
                        )
        ]


commentTests : Test
commentTests =
    Test.describe "comments"
        [ lineCommentTests
        ]


lineCommentTests : Test
lineCommentTests =
    Test.describe "line comments"
        [ Test.test "at the beginning" <|
            \() ->
                """
// Comment
main() {}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body = []
                                }
                            ]
                        )
        , Test.test "on the main line" <|
            \() ->
                """
main() {} // Comment
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body = []
                                }
                            ]
                        )
        , Test.test "inside main" <|
            \() ->
                """
main() {
 // Comment
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body = []
                                }
                            ]
                        )
        , Test.test "on main ending line" <|
            \() ->
                """
main() {
} // Comment
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body = []
                                }
                            ]
                        )
        , Test.test "after main" <|
            \() ->
                """
main() {
}
// Comment
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body = []
                                }
                            ]
                        )
        , Test.test "code gets preserved" <|
            \() ->
                """
main() {
 foo()
 // Comment
 bar()
}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body =
                                    [ CallStmt { fn = Var "foo", args = [] }
                                    , CallStmt { fn = Var "bar", args = [] }
                                    ]
                                }
                            ]
                        )
        , Test.test "between decls" <|
            \() ->
                """
main() {}
// Comment
foo() {}
"""
                    |> F80.Parser.parse
                    |> Expect.equal
                        (Ok
                            [ FnDecl
                                { name = "main"
                                , params = []
                                , body = []
                                }
                            , FnDecl
                                { name = "foo"
                                , params = []
                                , body = []
                                }
                            ]
                        )
        ]
