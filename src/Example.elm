module Example exposing
    ( assembly
    , ast
    , sourceCode
    )

import F80.AST
    exposing
        ( BinOp(..)
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Param
        , Stmt(..)
        , Value(..)
        )
import F80.Type


sourceCode : String
sourceCode =
    """
main() {
  ROM.clearScreen()
  let counter = 0
  renderStaticText()
  loop {
    renderCounter(counter)
    wait for keypress {
      Key.J -> { counter -= 1 }
      Key.K -> { counter += 1 }
    }
  }
}

renderStaticText() {
  Render.text(5, 5, "Counter: ")
  Render.text(5, 7, "Press J (-) or K (+)")
}

renderCounter(counter) {
  Render.text(14, 5, "   ")
  const counterStr = String.fromU8(counter)
  Render.text(14, 5, counterStr)
}
"""


ast : F80.AST.Program
ast =
    [ FnDecl
        { name = "main"
        , params = []
        , returnType = F80.Type.Unit
        , body =
            [ CallStmt
                { fn = "ROM.clearScreen"
                , args = []
                }
            , DefineLet
                { name = "counter"
                , value = Int 0
                }
            , CallStmt
                { fn = "renderStaticText"
                , args = []
                }
            , Loop
                [ CallStmt
                    { fn = "renderCounter"
                    , args = [ Var "counter" ]
                    }
                , WaitForKeypress
                    [ { on = KeyPattern_J
                      , body =
                            [ Assign
                                { var = "counter"
                                , op = Just BOp_Sub
                                , value = Int 1
                                }
                            ]
                      }
                    , { on = KeyPattern_K
                      , body =
                            [ Assign
                                { var = "counter"
                                , op = Just BOp_Add
                                , value = Int 1
                                }
                            ]
                      }
                    ]
                ]
            ]
        }
    , FnDecl
        { name = "renderStaticText"
        , params = []
        , returnType = F80.Type.Unit
        , body =
            [ CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 5
                    , Int 5
                    , String "Counter: "
                    ]
                }
            , CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 5
                    , Int 7
                    , String "Press J (-) or K (+)"
                    ]
                }
            ]
        }
    , FnDecl
        { name = "renderCounter"
        , params = [ Param "counter" F80.Type.U8 ]
        , returnType = F80.Type.Unit
        , body =
            [ CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 14
                    , Int 5
                    , String "   "
                    ]
                }
            , DefineConst
                { name = "counterStr"
                , value =
                    CallExpr
                        { fn = "String.fromU8"
                        , args = [ Var "counter" ]
                        }
                }
            , CallStmt
                { fn = "Render.text"
                , args =
                    [ Int 14
                    , Int 5
                    , Var "counterStr"
                    ]
                }
            ]
        }
    ]


assembly : String
assembly =
    """
    """
