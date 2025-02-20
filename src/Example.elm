module Example exposing (ast, emittedAsm, sourceCode)

import AST
    exposing
        ( BinOp(..)
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Stmt(..)
        )
import Asm


sourceCode : String
sourceCode =
    """
const counterLabel = "Counter: "
const textX = 5
const counterX = textX + String.length(counterLabel)
const counterY = 5
const helpY = counterY + 1

main() {
  let counter = 0
  let previousCounter = 0
  renderText()
  loop {
    renderCounter(counter, previousCounter)
    previousCounter = counter
    wait for keyboard {
      Key.Plus -> { counter += 1 }
      Key.Minus -> { if (counter > 0) { counter -= 1 } }
    }
  }
}

renderText() {
  Render.text(textX, counterY, counterLabel)
  Render.text(textX, helpY, "Press + or -")
}

renderCounter(counter, previous) {
  const counterStr = String.fromI16(counter)
  cleanupCounter(counterStr, previous)
  Render.text(counterX, counterY, counterStr)
}

// needed when going 10 -> 9, 100 -> 99, etc.
cleanupCounter(counterStr, previous) {
  const lenPrevious = String.length(String.fromI16(previous))
  const lenCounter = String.length(counterStr)
  if (lenCounter < lenPrevious) {
    Render.text(counterX + lenCounter, counterY, " ")
  }
}
"""


ast : AST.Program
ast =
    [ ConstDecl
        { name = "counterLabel"
        , expr = String "Counter: "
        }
    , ConstDecl
        { name = "textX"
        , expr = Int 5
        }
    , ConstDecl
        { name = "counterX"
        , expr =
            BinOp
                { op = BOp_Add
                , left = Var "textX"
                , right =
                    CallExpr
                        { fn = Var "String.length"
                        , args = [ Var "counterLabel" ]
                        }
                }
        }
    , ConstDecl
        { name = "counterY"
        , expr = Int 5
        }
    , ConstDecl
        { name = "helpY"
        , expr =
            BinOp
                { op = BOp_Add
                , left = Var "counterY"
                , right = Int 1
                }
        }
    , FnDecl
        { name = "main"
        , params = []
        , body =
            [ DefineLet
                { name = "counter"
                , value = Int 0
                }
            , DefineLet
                { name = "previousCounter"
                , value = Int 0
                }
            , CallStmt
                { fn = Var "renderText"
                , args = []
                }
            , Loop
                [ CallStmt
                    { fn = Var "renderCounter"
                    , args =
                        [ Var "counter"
                        , Var "previousCounter"
                        ]
                    }
                , Assign
                    { var = "previousCounter"
                    , op = Nothing
                    , value = Var "counter"
                    }
                , WaitForKeyboard
                    [ { on = KeyPattern_Plus
                      , body =
                            [ Assign
                                { var = "counter"
                                , op = Just BOp_Add
                                , value = Int 1
                                }
                            ]
                      }
                    , { on = KeyPattern_Minus
                      , body =
                            [ If
                                { cond =
                                    BinOp
                                        { op = BOp_Gt
                                        , left = Var "counter"
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
            ]
        }
    , FnDecl
        { name = "renderText"
        , params = []
        , body =
            [ CallStmt
                { fn = Var "Render.text"
                , args =
                    [ Var "textX"
                    , Var "counterY"
                    , Var "counterLabel"
                    ]
                }
            , CallStmt
                { fn = Var "Render.text"
                , args =
                    [ Var "textX"
                    , Var "helpY"
                    , String "Press + or -"
                    ]
                }
            ]
        }
    , FnDecl
        { name = "renderCounter"
        , params =
            [ "counter"
            , "previous"
            ]
        , body =
            [ DefineConst
                { name = "counterStr"
                , value =
                    CallExpr
                        { fn = Var "String.fromI16"
                        , args = [ Var "counter" ]
                        }
                }
            , CallStmt
                { fn = Var "cleanupCounter"
                , args =
                    [ Var "counterStr"
                    , Var "previous"
                    ]
                }
            , CallStmt
                { fn = Var "Render.text"
                , args =
                    [ Var "counterX"
                    , Var "counterY"
                    , Var "counterStr"
                    ]
                }
            ]
        }
    , FnDecl
        { name = "cleanupCounter"
        , params =
            [ "counterStr"
            , "previous"
            ]
        , body =
            [ DefineConst
                { name = "lenPrevious"
                , value =
                    CallExpr
                        { fn = Var "String.length"
                        , args =
                            [ CallExpr
                                { fn = Var "String.fromI16"
                                , args = [ Var "previous" ]
                                }
                            ]
                        }
                }
            , DefineConst
                { name = "lenCounter"
                , value =
                    CallExpr
                        { fn = Var "String.length"
                        , args = [ Var "counterStr" ]
                        }
                }
            , If
                { cond =
                    BinOp
                        { op = BOp_Lt
                        , left = Var "lenCounter"
                        , right = Var "lenPrevious"
                        }
                , then_ =
                    [ CallStmt
                        { fn = Var "Render.text"
                        , args =
                            [ BinOp
                                { op = BOp_Add
                                , left = Var "counterX"
                                , right = Var "lenCounter"
                                }
                            , Var "counterY"
                            , String " "
                            ]
                        }
                    ]
                , else_ = Nothing
                }
            ]
        }
    ]


emittedAsm : Asm.Program
emittedAsm =
    [ Asm.Label "main"

    -- TODO
    ]
