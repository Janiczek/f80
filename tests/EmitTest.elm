module EmitTest exposing (suite)

import Asm exposing (Line(..), Op(..))
import Emit
import Example
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import RegAlloc
import Test exposing (Test)


suite : Test
suite =
    Test.describe "F80.Emit.emit"
        [ Test.todo "some tests"
        , Test.todo "function call conventions"
        ]
