module TyperTest exposing (suite)

import AssocList as Dict exposing (Dict)
import Expect
import F80.AST exposing (Decl, Expr, Program, Stmt)
import F80.Lower
import F80.Parser
import F80.Path exposing (Path, Step(..))
import F80.Type exposing (Type(..))
import F80.Typer
import Test exposing (Test)


test_ : String -> List ( Path, Type ) -> Test
test_ sourceCode expected =
    Test.test ("Typing: " ++ sourceCode) <|
        \() ->
            case F80.Parser.parse sourceCode of
                Ok decls ->
                    decls
                        |> F80.Lower.lower
                        |> F80.Typer.findTypes
                        |> Result.map
                            (Dict.toList
                                >> List.sortBy (Tuple.first >> F80.Path.toString)
                            )
                        |> Expect.equal
                            (expected
                                |> List.sortBy (Tuple.first >> F80.Path.toString)
                                |> Ok
                            )

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


testErr : String -> F80.Typer.Error -> Test
testErr sourceCode expectedErr =
    Test.test ("Typing: " ++ sourceCode) <|
        \() ->
            case F80.Parser.parse sourceCode of
                Ok decls ->
                    decls
                        |> F80.Lower.lower
                        |> F80.Typer.findTypes
                        |> Expect.equal (Err expectedErr)

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.describe "F80.Typer.findTypes"
        [ globals
        , fnDecls
        , exprs
        ]


globals : Test
globals =
    Test.describe "globals"
        [ intGlobal
        , stringGlobal
        , varGlobal
        , boolGlobal
        , bytesGlobal
        , binopGlobal
        , unaryopGlobal
        , strlenGlobal
        ]


fnDecls : Test
fnDecls =
    Test.describe "fnDecls"
        [ mainIsSpecial
        ]


mainIsSpecial : Test
mainIsSpecial =
    Test.describe "main is special"
        [ testErr
            """
main(a: U8) {
    return a
}
            """
            { at = [ InDecl "main" ], type_ = F80.Typer.MainFnFunctionArityMismatch { actual = 1 } }
        , testErr
            """
main(): U8 {
    return 1
}
            """
            { at = [ InDecl "main" ], type_ = F80.Typer.MainFnReturnTypeMismatch { actual = U8 } }
        ]


exprs : Test
exprs =
    Test.describe "exprs"
        [ int
        , string
        ]


intGlobal : Test
intGlobal =
    Test.describe "int"
        [ test_
            """
const x = 1;            
            """
            [ ( [ InDecl "x" ], U8 ) ]
        ]


stringGlobal : Test
stringGlobal =
    Test.describe "string"
        [ test_
            """
const abc = "ABC"
            """
            [ ( [ InDecl "abc" ], String ) ]
        ]


varGlobal : Test
varGlobal =
    Test.describe "var"
        [ test_
            """
const x = 1
const y = x
            """
            [ ( [ InDecl "x" ], U8 )
            , ( [ InDecl "y" ], U8 )
            ]
        , test_
            """
const x = "abc"
const y = x
            """
            [ ( [ InDecl "x" ], String )
            , ( [ InDecl "y" ], String )
            ]
        ]


boolGlobal : Test
boolGlobal =
    Test.describe "bool"
        [ test_
            """
const x = true
const y = false
            """
            [ ( [ InDecl "x" ], Bool )
            , ( [ InDecl "y" ], Bool )
            ]
        ]


bytesGlobal : Test
bytesGlobal =
    Test.describe "bytes"
        [ test_
            """
const x = [1,2,3]
            """
            [ ( [ InDecl "x" ], Bytes ) ]
        ]


binopGlobal : Test
binopGlobal =
    Test.describe "binop"
        [ test_
            """
const x = 1 + 2
const y = 3 - 4
const z = 5 > 6
const w = 7 < 8
            """
            [ ( [ InDecl "x" ], U8 )
            , ( [ InDecl "y" ], U8 )
            , ( [ InDecl "z" ], Bool )
            , ( [ InDecl "w" ], Bool )
            ]
        , test_
            """
const x = 1
const y = x + 1
            """
            [ ( [ InDecl "x" ], U8 )
            , ( [ InDecl "y" ], U8 )
            ]
        ]


unaryopGlobal : Test
unaryopGlobal =
    Test.describe "unaryop"
        [ test_
            """
const y = !true
            """
            [ ( [ InDecl "y" ], Bool )
            ]
        ]


strlenGlobal : Test
strlenGlobal =
    Test.describe "strlen"
        [ test_
            """
const x = String.length("hello")
            """
            [ ( [ InDecl "x" ], U8 )
            ]
        , test_
            """
const x = String.length("hello")
const y = x
            """
            [ ( [ InDecl "x" ], U8 )
            , ( [ InDecl "y" ], U8 )
            ]
        ]


int : Test
int =
    Test.describe "int"
        [ test_
            """
main() {
    const x = 1
}
            """
            [ ( [ InConst "x", InDecl "main" ], U8 )
            , ( [ InDecl "main" ], Function [] Unit )
            ]
        ]


string : Test
string =
    Test.describe "string"
        [ test_
            """
main() {
    const abc = "ABC"
}
            """
            [ ( [ InDecl "_string_0_0" ], String )
            , ( [ InConst "abc", InDecl "main" ], String )
            , ( [ InDecl "main" ], Function [] Unit )
            ]
        ]
