module F80.AST.ToString exposing (toString)

import F80.AST
    exposing
        ( BinOp(..)
        , Block
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Program
        , Stmt(..)
        , Value(..)
        , WaitForKeyboardItem
        )


toString : Program -> String
toString decls =
    decls
        |> List.map declToString
        |> String.join "\n"


indent : String -> String
indent str =
    str
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


declToString : Decl -> String
declToString decl =
    case decl of
        GlobalDecl data ->
            "const " ++ data.name ++ " = " ++ valueToString data.value

        FnDecl data ->
            data.name
                ++ "("
                ++ String.join ", " data.params
                ++ ") "
                ++ blockToString data.body


valueToString : Value -> String
valueToString value =
    case value of
        VInt n ->
            String.fromInt n

        VString str ->
            -- TODO escape quotes
            "\"" ++ str ++ "\""

        VGlobal str ->
            str

        VBytes bytes ->
            "[ "
                ++ String.join ", " (List.map String.fromInt bytes)
                ++ " ]"

        VBinOp data ->
            valueToString data.left
                ++ " "
                ++ binOpToString data.op
                ++ " "
                ++ valueToString data.right

        VStringLength val ->
            "String.length(" ++ valueToString val ++ ")"


binOpToString : BinOp -> String
binOpToString op =
    case op of
        BOp_Add ->
            "+"

        BOp_Sub ->
            "-"

        BOp_Gt ->
            ">"

        BOp_Lt ->
            "<"


stmtToString : Stmt -> String
stmtToString stmt =
    case stmt of
        WaitForKeyboard items ->
            "wait for keyboard {\n"
                ++ (items
                        |> List.map (waitForKeyboardItemToString >> indent)
                        |> String.join "\n"
                   )
                ++ "\n}"

        Loop body ->
            "loop " ++ blockToString body

        If data ->
            "if ("
                ++ exprToString data.cond
                ++ ") "
                ++ blockToString data.then_
                ++ (case data.else_ of
                        Just else_ ->
                            " else " ++ blockToString else_

                        Nothing ->
                            ""
                   )

        DefineConst data ->
            "const "
                ++ data.name
                ++ " = "
                ++ exprToString data.value

        DefineLet data ->
            "let "
                ++ data.name
                ++ " = "
                ++ exprToString data.value

        Assign data ->
            data.var
                ++ " "
                ++ (case data.op of
                        Just op ->
                            binOpToString op

                        Nothing ->
                            ""
                   )
                ++ "= "
                ++ exprToString data.value

        CallStmt data ->
            data.fn
                ++ "("
                ++ String.join ", " (List.map exprToString data.args)
                ++ ")"


waitForKeyboardItemToString : WaitForKeyboardItem -> String
waitForKeyboardItemToString item =
    keyPatternToString item.on ++ " -> " ++ blockToString item.body


blockToString : Block -> String
blockToString body =
    "{\n"
        ++ (body
                |> List.map (stmtToString >> indent)
                |> String.join "\n"
           )
        ++ "\n}"


keyPatternToString : KeyPattern -> String
keyPatternToString pattern =
    case pattern of
        KeyPattern_Plus ->
            "+"

        KeyPattern_Minus ->
            "-"


exprToString : Expr -> String
exprToString expr =
    case expr of
        Var str ->
            str

        Int n ->
            String.fromInt n

        String str ->
            -- TODO escape quotes
            "\"" ++ str ++ "\""

        BinOp data ->
            exprToString data.left
                ++ " "
                ++ binOpToString data.op
                ++ " "
                ++ exprToString data.right

        CallExpr data ->
            data.fn
                ++ "("
                ++ String.join ", " (List.map exprToString data.args)
                ++ ")"

        IfExpr data ->
            "if ("
                ++ exprToString data.cond
                ++ ") "
                ++ exprToString data.then_
                ++ " else "
                ++ exprToString data.else_
