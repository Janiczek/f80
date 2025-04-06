module CliTest exposing (suite)

import Expect
import F80
import Test exposing (Test)


suite : Test
suite =
    Test.test "F80 (CLI)" <|
        \() ->
            let
                _ =
                    F80.main
            in
            -- Compiles!
            Expect.pass
