module SSATest exposing (suite)

import AST
    exposing
        ( BinOp(..)
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Stmt(..)
        )
import Example
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import SSA
import Test exposing (Test)


suite : Test
suite =
    Test.describe "SSA.translate"
        [ Test.todo "some tests"
        ]
