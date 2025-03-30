module F80.Typer exposing (Ctx, ExprId, findTypes)

{-| Uses bidirectional type inference.
-}

import Dict exposing (Dict)
import F80.AST
import F80.Type exposing (Type(..))


type alias ExprId =
    List String


type alias Ctx =
    Dict ExprId Type


type alias Error =
    { at : ExprId
    , type_ : ErrorType
    }


type ErrorType
    = TypeMismatch { expected : Type, actual : Type }
    | GlobalNotFound { name : String }
    | FunctionNotFound { name : String }
    | GlobalNotFunction { name : String, actual : Type }
    | FunctionArityMismatch { expected : Int, actual : Int }
    | VarNotFound { name : String }


findTypes : F80.AST.Program -> Result Error Ctx
findTypes program =
    let
        processDecl : F80.AST.Decl -> Ctx -> Result Error Ctx
        processDecl decl ctx1 =
            let
                -- all decls are on the top level
                parentId : ExprId
                parentId =
                    []
            in
            case decl of
                F80.AST.GlobalDecl data ->
                    -- TODO check there's only one global decl of this name
                    case inferValue ctx1 parentId data.value of
                        Err err ->
                            Debug.todo "infer value result err"

                        Ok type_ ->
                            Ok (Dict.insert [ data.name ] type_ ctx1)

                F80.AST.FnDecl data ->
                    -- TODO check there's only one fn decl of this name
                    let
                        checkFnDecl : Ctx -> Result Error Ctx
                        checkFnDecl ctx_ =
                            let
                                fnCtx : Ctx
                                fnCtx =
                                    List.foldl
                                        (\param ctx__ ->
                                            Dict.insert (param.name :: data.name :: parentId) U8 ctx__
                                        )
                                        (Dict.insert [ data.name ]
                                            (F80.Type.Function
                                                (List.map .type_ data.params)
                                                data.returnType
                                            )
                                            ctx_
                                        )
                                        data.params
                            in
                            case checkBlock fnCtx (data.name :: parentId) data.body of
                                Err err ->
                                    Err err

                                Ok ctx2 ->
                                    Ok ctx2

                        checkFnDeclArity : Int -> Ctx -> Result Error Ctx
                        checkFnDeclArity expectedArity ctx_ =
                            let
                                actualArity =
                                    List.length data.params
                            in
                            if actualArity /= expectedArity then
                                Err
                                    { at = parentId
                                    , type_ =
                                        FunctionArityMismatch
                                            { expected = expectedArity
                                            , actual = actualArity
                                            }
                                    }

                            else
                                Ok ctx_

                        checkFnDeclReturnType : Type -> Ctx -> Result Error Ctx
                        checkFnDeclReturnType expectedReturnType ctx_ =
                            if data.returnType /= expectedReturnType then
                                Err
                                    { at = parentId
                                    , type_ =
                                        TypeMismatch
                                            { expected = expectedReturnType
                                            , actual = data.returnType
                                            }
                                    }

                            else
                                Ok ctx_
                    in
                    if F80.AST.isMain data.name then
                        Ok ctx1
                            |> Result.andThen (checkFnDeclArity 0)
                            |> Result.andThen (checkFnDeclReturnType Unit)
                            |> Result.andThen checkFnDecl

                    else
                        checkFnDecl ctx1

        result =
            List.foldl
                (\decl acc ->
                    case acc of
                        Ok ctx1 ->
                            processDecl decl ctx1

                        Err err ->
                            Err err
                )
                (Ok Dict.empty)
                program
    in
    result


inferValue : Ctx -> ExprId -> F80.AST.Value -> Result Error Type
inferValue ctx parentId value =
    case value of
        F80.AST.VInt _ ->
            Ok U8

        F80.AST.VString _ ->
            Ok String

        F80.AST.VBool _ ->
            Ok Bool

        F80.AST.VGlobal name ->
            case Dict.get [ name ] ctx of
                Nothing ->
                    Err { at = parentId, type_ = GlobalNotFound { name = name } }

                Just type_ ->
                    Ok type_

        F80.AST.VBytes _ ->
            Ok Bytes

        F80.AST.VBinOp data ->
            let
                binOpId : String
                binOpId =
                    "binop"

                newParentId : ExprId
                newParentId =
                    binOpId :: parentId
            in
            case
                ( data.op
                , inferValue ctx newParentId data.left
                , inferValue ctx newParentId data.right
                )
            of
                ( _, Err leftErr, _ ) ->
                    Err leftErr

                ( _, _, Err rightErr ) ->
                    Err rightErr

                ( F80.AST.BOp_Add, Ok U8, Ok U8 ) ->
                    Ok U8

                ( F80.AST.BOp_Add, Ok U8, Ok other ) ->
                    Err { at = "right" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Add, Ok other, Ok _ ) ->
                    Err { at = "left" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Sub, Ok U8, Ok U8 ) ->
                    Ok U8

                ( F80.AST.BOp_Sub, Ok U8, Ok other ) ->
                    Err { at = "right" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Sub, Ok other, Ok _ ) ->
                    Err { at = "left" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Gt, Ok U8, Ok U8 ) ->
                    Ok Bool

                ( F80.AST.BOp_Gt, Ok U8, Ok other ) ->
                    Err { at = "right" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Gt, Ok other, Ok _ ) ->
                    Err { at = "left" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Lt, Ok U8, Ok U8 ) ->
                    Ok Bool

                ( F80.AST.BOp_Lt, Ok U8, Ok other ) ->
                    Err { at = "right" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Lt, Ok other, Ok _ ) ->
                    Err { at = "left" :: newParentId, type_ = TypeMismatch { expected = U8, actual = other } }

        F80.AST.VUnaryOp data ->
            let
                unaryOpId : String
                unaryOpId =
                    "unaryop"

                newParentId : ExprId
                newParentId =
                    unaryOpId :: parentId
            in
            case ( data.op, inferValue ctx newParentId data.value ) of
                ( _, Err err ) ->
                    Err err

                ( F80.AST.UOp_Not, Ok Bool ) ->
                    Ok Bool

                ( F80.AST.UOp_Not, Ok other ) ->
                    Err { at = newParentId, type_ = TypeMismatch { expected = Bool, actual = other } }

        F80.AST.VStringLength val ->
            case inferValue ctx parentId val of
                Err err ->
                    Err err

                Ok String ->
                    Ok U8

                Ok other ->
                    Err { at = parentId, type_ = TypeMismatch { expected = String, actual = other } }


checkBlock : Ctx -> ExprId -> F80.AST.Block -> Result Error Ctx
checkBlock ctx parentId block =
    List.foldl
        (\stmt resultAcc ->
            case resultAcc of
                Err err ->
                    Err err

                Ok ctx1 ->
                    checkStmt ctx1 parentId stmt
        )
        (Ok ctx)
        block


checkStmt : Ctx -> ExprId -> F80.AST.Stmt -> Result Error Ctx
checkStmt ctx parentId stmt =
    case stmt of
        F80.AST.WaitForKeypress items ->
            List.foldl
                (\item resultAcc ->
                    case resultAcc of
                        Err err ->
                            Err err

                        Ok ctx1 ->
                            checkBlock ctx1 parentId item.body
                )
                (Ok ctx)
                items

        F80.AST.Loop block ->
            checkBlock ctx parentId block

        F80.AST.If data ->
            case checkExpr ctx parentId data.cond Bool of
                Err err ->
                    Err err

                Ok () ->
                    case checkBlock ctx parentId data.then_ of
                        Err err ->
                            Err err

                        Ok ctx2 ->
                            case data.else_ of
                                Nothing ->
                                    Ok ctx2

                                Just else_ ->
                                    checkBlock ctx2 parentId else_

        F80.AST.DefineConst data ->
            case inferExpr ctx parentId data.value of
                Err err ->
                    Err err

                Ok ( ctx1, type_ ) ->
                    Ok (Dict.insert (data.name :: parentId) type_ ctx1)

        F80.AST.DefineLet data ->
            case inferExpr ctx parentId data.value of
                Err err ->
                    Err err

                Ok ( ctx1, type_ ) ->
                    Ok (Dict.insert (data.name :: parentId) type_ ctx1)

        F80.AST.Assign data ->
            let
                inferResult =
                    case Dict.get (data.var :: parentId) ctx of
                        Nothing ->
                            inferExpr ctx parentId data.value

                        Just varType ->
                            case checkExpr ctx parentId data.value varType of
                                Err err ->
                                    Err err

                                Ok () ->
                                    Ok ( ctx, varType )
            in
            case inferResult of
                Err err ->
                    Err err

                Ok ( ctx1, type_ ) ->
                    Ok (Dict.insert (data.var :: parentId) type_ ctx1)

        F80.AST.CallStmt data ->
            case checkCall ctx parentId data of
                Err err ->
                    Err err

                Ok () ->
                    Ok ctx

        F80.AST.Return maybeExpr ->
            case maybeExpr of
                Nothing ->
                    Ok ctx

                Just expr ->
                    case inferExpr ctx parentId expr of
                        Err err ->
                            Err err

                        Ok ( ctx1, type_ ) ->
                            Ok ctx1


inferExpr : Ctx -> ExprId -> F80.AST.Expr -> Result Error ( Ctx, Type )
inferExpr ctx parentId expr =
    case expr of
        F80.AST.Var id ->
            case Dict.get (id :: parentId) ctx of
                Nothing ->
                    Err { at = parentId, type_ = VarNotFound { name = id } }

                Just type_ ->
                    Ok ( ctx, type_ )

        F80.AST.Int _ ->
            Ok ( ctx, U8 )

        F80.AST.String _ ->
            Ok ( ctx, String )

        F80.AST.Bool _ ->
            Ok ( ctx, Bool )

        F80.AST.BinOp data ->
            Debug.todo "infer binop"

        F80.AST.UnaryOp data ->
            Debug.todo "infer unaryop"

        F80.AST.CallExpr data ->
            Debug.todo "infer call"

        F80.AST.IfExpr data ->
            Debug.todo "infer if"


checkExpr : Ctx -> ExprId -> F80.AST.Expr -> Type -> Result Error ()
checkExpr ctx parentId expr type_ =
    case expr of
        F80.AST.Var id ->
            checkEqual parentId type_ U8

        F80.AST.Int _ ->
            checkEqual parentId type_ U8

        F80.AST.String _ ->
            checkEqual parentId type_ String

        F80.AST.Bool _ ->
            checkEqual parentId type_ Bool

        F80.AST.BinOp data ->
            case data.op of
                F80.AST.BOp_Add ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentId data.left U8)
                        (checkExpr ctx parentId data.right U8)
                        (checkEqual parentId type_ U8)

                F80.AST.BOp_Sub ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentId data.left U8)
                        (checkExpr ctx parentId data.right U8)
                        (checkEqual parentId type_ U8)

                F80.AST.BOp_Gt ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentId data.left U8)
                        (checkExpr ctx parentId data.right U8)
                        (checkEqual parentId type_ Bool)

                F80.AST.BOp_Lt ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentId data.left U8)
                        (checkExpr ctx parentId data.right U8)
                        (checkEqual parentId type_ Bool)

        F80.AST.UnaryOp data ->
            case data.op of
                F80.AST.UOp_Not ->
                    checkExpr ctx parentId data.expr Bool
                        |> Result.andThen (\() -> checkEqual parentId type_ Bool)

        F80.AST.CallExpr data ->
            checkCall ctx parentId data

        F80.AST.IfExpr data ->
            checkExpr ctx parentId data.cond type_
                |> Result.andThen
                    (\() ->
                        checkExpr ctx parentId data.then_ type_
                            |> Result.andThen
                                (\() ->
                                    checkExpr ctx parentId data.else_ type_
                                )
                    )


checkCall : Ctx -> ExprId -> F80.AST.CallData -> Result Error ()
checkCall ctx parentId data =
    case Dict.get [ data.fn ] ctx of
        Nothing ->
            Err { at = parentId, type_ = FunctionNotFound { name = data.fn } }

        Just ((Function paramsType restType) as fnType) ->
            let
                expectedArity =
                    List.length paramsType

                actualArity =
                    List.length data.args
            in
            if expectedArity /= actualArity then
                Err
                    { at = parentId
                    , type_ =
                        FunctionArityMismatch
                            { expected = expectedArity
                            , actual = actualArity
                            }
                    }

            else
                List.map2 Tuple.pair data.args paramsType
                    |> List.foldl
                        (\( arg, paramType ) accResult ->
                            case accResult of
                                Err err ->
                                    Err err

                                Ok () ->
                                    case checkExpr ctx parentId arg paramType of
                                        Ok () ->
                                            Ok ()

                                        Err err ->
                                            Err err
                        )
                        (Ok ())

        Just actual ->
            Err { at = parentId, type_ = GlobalNotFunction { name = data.fn, actual = actual } }


checkEqual : ExprId -> Type -> Type -> Result Error ()
checkEqual parentId expected actual =
    if expected == actual then
        Ok ()

    else
        Err { at = parentId, type_ = TypeMismatch { expected = expected, actual = actual } }
