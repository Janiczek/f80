module F80.AST.ToString exposing (exprToString, toString)

import F80.AST
    exposing
        ( BinOp(..)
        , Block
        , Decl(..)
        , Expr(..)
        , KeyPattern(..)
        , Program
        , Stmt(..)
        , UnaryOp(..)
        , Value(..)
        , WaitForKeypressItem
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

        VBool bool ->
            if bool then
                "true"

            else
                "false"

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

        VUnaryOp data ->
            -- Assuming they're all prefix
            unaryOpToString data.op
                ++ " "
                ++ valueToString data.expr

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


unaryOpToString : UnaryOp -> String
unaryOpToString op =
    case op of
        UOp_Neg ->
            "-"

        UOp_Not ->
            "!"


stmtToString : Stmt -> String
stmtToString stmt =
    case stmt of
        WaitForKeypress items ->
            "wait for keypress {\n"
                ++ (items
                        |> List.map (waitForKeypressItemToString >> indent)
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

        Return maybeExpr ->
            case maybeExpr of
                Nothing ->
                    "return"

                Just expr ->
                    "return " ++ exprToString expr


waitForKeypressItemToString : WaitForKeypressItem -> String
waitForKeypressItemToString item =
    keyPatternToString item.on ++ " -> " ++ blockToString item.body


blockToString : Block -> String
blockToString body =
    [ [ "{" ]
    , List.map (stmtToString >> indent) body
    , [ "}" ]
    ]
        |> List.concat
        |> String.join "\n"


keyPatternToString : KeyPattern -> String
keyPatternToString pattern =
    case pattern of
        KeyPattern_J ->
            "Key.J"

        KeyPattern_K ->
            "Key.K"


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

        Bool bool ->
            if bool then
                "true"

            else
                "false"

        BinOp data ->
            exprToString data.left
                ++ " "
                ++ binOpToString data.op
                ++ " "
                ++ exprToString data.right

        UnaryOp data ->
            -- Assuming they're all prefix
            unaryOpToString data.op
                ++ " "
                ++ exprToString data.expr

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
