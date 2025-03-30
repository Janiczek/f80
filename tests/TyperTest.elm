module TyperTest exposing (suite)

import Dict
import Expect
import F80.AST exposing (Decl, Expr, Program, Stmt)
import F80.Lower
import F80.Parser
import F80.Type exposing (Type(..))
import F80.Typer exposing (ExprId)
import Test exposing (Test)


test_ : String -> List ( ExprId, Type ) -> Test
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
                                >> List.sortBy Tuple.first
                            )
                        |> Expect.equal
                            (expected
                                |> List.sortBy Tuple.first
                                |> Ok
                            )

                Err err ->
                    Expect.fail ("Failed to parse source: " ++ Debug.toString err)


suite : Test
suite =
    Test.only <|
        Test.describe "F80.Typer.findTypes"
            [ globals
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
            [ ( [ "x" ], U8 ) ]
        ]


stringGlobal : Test
stringGlobal =
    Test.describe "string"
        [ test_
            """
const abc = "ABC"
            """
            [ ( [ "abc" ], String ) ]
        ]


varGlobal : Test
varGlobal =
    Test.describe "var"
        [ test_
            """
const x = 1
const y = x
            """
            [ ( [ "x" ], U8 )
            , ( [ "y" ], U8 )
            ]
        , test_
            """
const x = "abc"
const y = x
            """
            [ ( [ "x" ], String )
            , ( [ "y" ], String )
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
            [ ( [ "x" ], Bool )
            , ( [ "y" ], Bool )
            ]
        ]


bytesGlobal : Test
bytesGlobal =
    Test.describe "bytes"
        [ test_
            """
const x = [1,2,3]
            """
            [ ( [ "x" ], Bytes ) ]
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
            [ ( [ "x" ], U8 )
            , ( [ "y" ], U8 )
            , ( [ "z" ], Bool )
            , ( [ "w" ], Bool )
            ]
        , test_
            """
const x = 1
const y = x + 1
            """
            [ ( [ "x" ], U8 )
            , ( [ "y" ], U8 )
            ]
        ]


unaryopGlobal : Test
unaryopGlobal =
    Test.describe "unaryop"
        [ test_
            """
const y = !true
            """
            [ ( [ "y" ], Bool )
            ]
        ]


strlenGlobal : Test
strlenGlobal =
    Test.describe "strlen"
        [ test_
            """
const x = String.length("hello")
            """
            [ ( [ "x" ], U8 )
            ]
        , test_
            """
const x = String.length("hello")
const y = x
            """
            [ ( [ "x" ], U8 )
            , ( [ "y" ], U8 )
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
            [ ( [ "x", "main" ], U8 )
            , ( [ "main" ], Function [] Unit )
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
            [ ( [ "abc", "main" ], String )
            , ( [ "main" ], Function [] Unit )
            ]
        ]
