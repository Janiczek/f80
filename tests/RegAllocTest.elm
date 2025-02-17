module RegAllocTest exposing (suite)

import Example
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import RegAlloc
import SSA
import Test exposing (Test)


suite : Test
suite =
    Test.describe "RegAlloc.allocate"
        [ Test.todo "some tests"
        ]
