module F80.Typer exposing (Ctx, Error, ErrorType(..), findTypes, findVar)

{-| Uses bidirectional type inference.
-}

import AssocList as Dict exposing (Dict)
import F80.AST
import F80.Path exposing (Path, Step(..))
import F80.Type exposing (Type(..))
import List.Extra
import Maybe.Extra


type alias Ctx =
    Dict Path Type


type alias Error =
    { at : Path
    , type_ : ErrorType
    }


type ErrorType
    = TypeMismatch { expected : Type, actual : Type }
    | ReturnTypeMismatch { expected : Type, actual : Type }
    | GlobalNotFound { name : String }
    | FunctionNotFound { name : String }
    | GlobalNotFunction { name : String, actual : Type }
    | FunctionArityMismatch { expected : Int, actual : Int }
    | VarNotFound { name : String }
    | MainFnReturnTypeMismatch { actual : Type }
    | MainFnFunctionArityMismatch { actual : Int }


prelude : List ( String, Type )
prelude =
    [ ( "Render.text", Function [ U8, U8, String ] Unit )
    , ( "ROM.clearScreen", Function [] Unit )
    ]


initCtx : Ctx
initCtx =
    Dict.fromList []


findTypes : F80.AST.Program -> Result Error Ctx
findTypes program =
    let
        -- all decls are on the top level
        rootPath : Path
        rootPath =
            []

        processDecl : F80.AST.Decl -> Ctx -> Result Error Ctx
        processDecl decl ctx1 =
            case decl of
                F80.AST.GlobalDecl data ->
                    -- TODO check there's only one global decl of this name
                    case inferValue ctx1 rootPath data.value of
                        Err err ->
                            Debug.todo "infer value result err"

                        Ok type_ ->
                            Ok (Dict.insert [ InDecl data.name ] type_ ctx1)

                F80.AST.FnDecl data ->
                    -- TODO check there's only one fn decl of this name
                    let
                        declPath : Path
                        declPath =
                            InDecl data.name :: rootPath

                        checkFnDecl : Ctx -> Result Error Ctx
                        checkFnDecl ctx_ =
                            let
                                fnCtx : Ctx
                                fnCtx =
                                    List.foldl
                                        (\param ctx__ ->
                                            Dict.insert (InParam param.name :: declPath) U8 ctx__
                                        )
                                        (Dict.insert declPath
                                            (F80.Type.Function
                                                (List.map .type_ data.params)
                                                data.returnType
                                            )
                                            ctx_
                                        )
                                        data.params
                            in
                            case checkBlock fnCtx declPath data.body of
                                Err err ->
                                    Err err

                                Ok ctx2 ->
                                    Ok ctx2

                        checkMainFnDeclArity : String -> Ctx -> Result Error Ctx
                        checkMainFnDeclArity declName ctx_ =
                            let
                                actualArity =
                                    List.length data.params
                            in
                            if actualArity /= 0 then
                                Err
                                    { at = declPath
                                    , type_ = MainFnFunctionArityMismatch { actual = actualArity }
                                    }

                            else
                                Ok ctx_

                        checkMainFnDeclReturnType : String -> Ctx -> Result Error Ctx
                        checkMainFnDeclReturnType declName ctx_ =
                            if data.returnType /= Unit then
                                Err
                                    { at = declPath
                                    , type_ = MainFnReturnTypeMismatch { actual = data.returnType }
                                    }

                            else
                                Ok ctx_
                    in
                    if F80.AST.isMain data.name then
                        Ok ctx1
                            |> Result.andThen (checkMainFnDeclArity data.name)
                            |> Result.andThen (checkMainFnDeclReturnType data.name)
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
                (Ok initCtx)
                program
    in
    result


inferValue : Ctx -> Path -> F80.AST.Value -> Result Error Type
inferValue ctx parentPath value =
    case value of
        F80.AST.VInt _ ->
            Ok U8

        F80.AST.VString _ ->
            Ok String

        F80.AST.VBool _ ->
            Ok Bool

        F80.AST.VGlobal name ->
            case Dict.get [ InDecl name ] ctx of
                Nothing ->
                    Err
                        { at = parentPath
                        , type_ = GlobalNotFound { name = name }
                        }

                Just type_ ->
                    Ok type_

        F80.AST.VBytes _ ->
            Ok Bytes

        F80.AST.VBinOp data ->
            let
                vbinopPath : Path
                vbinopPath =
                    InVBinOp :: parentPath
            in
            case
                ( data.op
                , inferValue ctx vbinopPath data.left
                , inferValue ctx vbinopPath data.right
                )
            of
                ( _, Err leftErr, _ ) ->
                    Err leftErr

                ( _, _, Err rightErr ) ->
                    Err rightErr

                ( F80.AST.BOp_Add, Ok U8, Ok U8 ) ->
                    Ok U8

                ( F80.AST.BOp_Add, Ok U8, Ok other ) ->
                    Err { at = InVBinOpRight :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Add, Ok other, Ok _ ) ->
                    Err { at = InVBinOpLeft :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Sub, Ok U8, Ok U8 ) ->
                    Ok U8

                ( F80.AST.BOp_Sub, Ok U8, Ok other ) ->
                    Err { at = InVBinOpRight :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Sub, Ok other, Ok _ ) ->
                    Err { at = InVBinOpLeft :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Gt, Ok U8, Ok U8 ) ->
                    Ok Bool

                ( F80.AST.BOp_Gt, Ok U8, Ok other ) ->
                    Err { at = InVBinOpRight :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Gt, Ok other, Ok _ ) ->
                    Err { at = InVBinOpLeft :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Lt, Ok U8, Ok U8 ) ->
                    Ok Bool

                ( F80.AST.BOp_Lt, Ok U8, Ok other ) ->
                    Err { at = InVBinOpRight :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

                ( F80.AST.BOp_Lt, Ok other, Ok _ ) ->
                    Err { at = InVBinOpLeft :: vbinopPath, type_ = TypeMismatch { expected = U8, actual = other } }

        F80.AST.VUnaryOp data ->
            let
                vunaryopPath : Path
                vunaryopPath =
                    InVUnaryOp :: parentPath
            in
            case ( data.op, inferValue ctx vunaryopPath data.value ) of
                ( _, Err err ) ->
                    Err err

                ( F80.AST.UOp_Not, Ok Bool ) ->
                    Ok Bool

                ( F80.AST.UOp_Not, Ok other ) ->
                    Err { at = vunaryopPath, type_ = TypeMismatch { expected = Bool, actual = other } }

        F80.AST.VStringLength val ->
            case inferValue ctx parentPath val of
                Err err ->
                    Err err

                Ok String ->
                    Ok U8

                Ok other ->
                    Err { at = parentPath, type_ = TypeMismatch { expected = String, actual = other } }


checkBlock : Ctx -> Path -> F80.AST.Block -> Result Error Ctx
checkBlock ctx parentPath block =
    List.foldl
        (\stmt resultAcc ->
            case resultAcc of
                Err err ->
                    Err err

                Ok ctx1 ->
                    checkStmt ctx1 parentPath stmt
        )
        (Ok ctx)
        block


checkStmt : Ctx -> Path -> F80.AST.Stmt -> Result Error Ctx
checkStmt ctx parentPath stmt =
    case stmt of
        F80.AST.WaitForKeypress items ->
            List.foldl
                (\item resultAcc ->
                    case resultAcc of
                        Err err ->
                            Err err

                        Ok ctx1 ->
                            checkBlock ctx1 parentPath item.body
                )
                (Ok ctx)
                items

        F80.AST.Loop block ->
            checkBlock ctx parentPath block

        F80.AST.If data ->
            case checkExpr ctx parentPath data.cond Bool of
                Err err ->
                    Err err

                Ok () ->
                    case checkBlock ctx parentPath data.then_ of
                        Err err ->
                            Err err

                        Ok ctx2 ->
                            case data.else_ of
                                Nothing ->
                                    Ok ctx2

                                Just else_ ->
                                    checkBlock ctx2 parentPath else_

        F80.AST.DefineConst data ->
            case inferExpr ctx parentPath data.value of
                Err err ->
                    Err err

                Ok ( ctx1, type_ ) ->
                    Ok (Dict.insert (InConst data.name :: parentPath) type_ ctx1)

        F80.AST.DefineLet data ->
            case inferExpr ctx parentPath data.value of
                Err err ->
                    Err err

                Ok ( ctx1, type_ ) ->
                    Ok (Dict.insert (InLet data.name :: parentPath) type_ ctx1)

        F80.AST.Assign data ->
            let
                varPath : Path
                varPath =
                    -- We can only assign to a let variable, not to a const or decl one
                    InLet data.var :: parentPath

                inferResult =
                    case Dict.get varPath ctx of
                        Nothing ->
                            inferExpr ctx parentPath data.value

                        Just varType ->
                            case checkExpr ctx parentPath data.value varType of
                                Err err ->
                                    Err err

                                Ok () ->
                                    Ok ( ctx, varType )
            in
            case inferResult of
                Err err ->
                    Err err

                Ok ( ctx1, type_ ) ->
                    Ok (Dict.insert varPath type_ ctx1)

        F80.AST.CallStmt data ->
            let
                ctx_ =
                    updateCtxForPreludeCall data.fn ctx
            in
            case checkCall ctx_ parentPath data of
                Err err ->
                    Err err

                Ok () ->
                    Ok ctx_

        F80.AST.Return maybeExpr ->
            case maybeExpr of
                Nothing ->
                    Ok ctx

                Just expr ->
                    case inferExpr ctx parentPath expr of
                        Err err ->
                            Err err

                        Ok ( ctx1, type_ ) ->
                            Ok ctx1


findVar : String -> Path -> Ctx -> Maybe Type
findVar var path ctx_ =
    case
        Maybe.Extra.orList
            [ Dict.get (InDecl var :: path) ctx_ -- global variable
            , Dict.get (InParam var :: path) ctx_
            , Dict.get (InLet var :: path) ctx_
            , Dict.get (InConst var :: path) ctx_
            , Dict.get (InVar var :: path) ctx_ -- TODO: unclear if this is needed
            ]
    of
        Just type_ ->
            Just type_

        Nothing ->
            case List.Extra.unconsLast path of
                Nothing ->
                    Nothing

                Just ( _, path_ ) ->
                    findVar var path_ ctx_


inferExpr : Ctx -> Path -> F80.AST.Expr -> Result Error ( Ctx, Type )
inferExpr ctx parentPath expr =
    case expr of
        F80.AST.Var id ->
            case findVar id parentPath ctx of
                Nothing ->
                    Err { at = parentPath, type_ = VarNotFound { name = id } }

                Just type_ ->
                    Ok ( ctx, type_ )

        F80.AST.Int _ ->
            Ok ( ctx, U8 )

        F80.AST.String _ ->
            Ok ( ctx, String )

        F80.AST.Bool _ ->
            Ok ( ctx, Bool )

        F80.AST.BinOp data ->
            inferExpr ctx parentPath data.left
                |> Result.andThen
                    (\( ctx1, leftType ) ->
                        inferExpr ctx1 parentPath data.right
                            |> Result.andThen
                                (\( ctx2, rightType ) ->
                                    case ( data.op, leftType, rightType ) of
                                        ( F80.AST.BOp_Add, U8, U8 ) ->
                                            Ok ( ctx2, U8 )

                                        ( F80.AST.BOp_Add, U8, _ ) ->
                                            Err { at = InBinOpRight :: parentPath, type_ = TypeMismatch { expected = U8, actual = rightType } }

                                        ( F80.AST.BOp_Add, _, _ ) ->
                                            Err { at = InBinOpLeft :: parentPath, type_ = TypeMismatch { expected = U8, actual = leftType } }

                                        ( F80.AST.BOp_Sub, U8, U8 ) ->
                                            Ok ( ctx2, U8 )

                                        ( F80.AST.BOp_Sub, U8, _ ) ->
                                            Err { at = InBinOpRight :: parentPath, type_ = TypeMismatch { expected = U8, actual = rightType } }

                                        ( F80.AST.BOp_Sub, _, _ ) ->
                                            Err { at = InBinOpLeft :: parentPath, type_ = TypeMismatch { expected = U8, actual = leftType } }

                                        ( F80.AST.BOp_Gt, U8, U8 ) ->
                                            Ok ( ctx2, Bool )

                                        ( F80.AST.BOp_Gt, U8, _ ) ->
                                            Err { at = InBinOpRight :: parentPath, type_ = TypeMismatch { expected = U8, actual = rightType } }

                                        ( F80.AST.BOp_Gt, _, _ ) ->
                                            Err { at = InBinOpLeft :: parentPath, type_ = TypeMismatch { expected = U8, actual = leftType } }

                                        ( F80.AST.BOp_Lt, U8, U8 ) ->
                                            Ok ( ctx2, Bool )

                                        ( F80.AST.BOp_Lt, U8, _ ) ->
                                            Err { at = InBinOpRight :: parentPath, type_ = TypeMismatch { expected = U8, actual = rightType } }

                                        ( F80.AST.BOp_Lt, _, _ ) ->
                                            Err { at = InBinOpLeft :: parentPath, type_ = TypeMismatch { expected = U8, actual = leftType } }
                                )
                    )

        F80.AST.UnaryOp data ->
            inferExpr ctx parentPath data.expr
                |> Result.andThen
                    (\( ctx1, type_ ) ->
                        case ( data.op, type_ ) of
                            ( F80.AST.UOp_Not, Bool ) ->
                                Ok ( ctx1, Bool )

                            ( F80.AST.UOp_Not, _ ) ->
                                Err { at = InUnaryOp :: parentPath, type_ = TypeMismatch { expected = Bool, actual = type_ } }
                    )

        F80.AST.CallExpr data ->
            Debug.todo "infer call - don't forget to add Render.text if used"

        F80.AST.IfExpr data ->
            Debug.todo "infer if"


checkExpr : Ctx -> Path -> F80.AST.Expr -> Type -> Result Error ()
checkExpr ctx parentPath expr type_ =
    case expr of
        F80.AST.Var id ->
            case findVar id parentPath ctx of
                Nothing ->
                    Err { at = parentPath, type_ = VarNotFound { name = id } }

                Just varType ->
                    checkEqual parentPath type_ varType

        F80.AST.Int _ ->
            checkEqual parentPath type_ U8

        F80.AST.String _ ->
            checkEqual parentPath type_ String

        F80.AST.Bool _ ->
            checkEqual parentPath type_ Bool

        F80.AST.BinOp data ->
            case data.op of
                F80.AST.BOp_Add ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentPath data.left U8)
                        (checkExpr ctx parentPath data.right U8)
                        (checkEqual parentPath type_ U8)

                F80.AST.BOp_Sub ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentPath data.left U8)
                        (checkExpr ctx parentPath data.right U8)
                        (checkEqual parentPath type_ U8)

                F80.AST.BOp_Gt ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentPath data.left U8)
                        (checkExpr ctx parentPath data.right U8)
                        (checkEqual parentPath type_ Bool)

                F80.AST.BOp_Lt ->
                    Result.map3 (\() () () -> ())
                        (checkExpr ctx parentPath data.left U8)
                        (checkExpr ctx parentPath data.right U8)
                        (checkEqual parentPath type_ Bool)

        F80.AST.UnaryOp data ->
            case data.op of
                F80.AST.UOp_Not ->
                    checkExpr ctx parentPath data.expr Bool
                        |> Result.andThen (\() -> checkEqual parentPath type_ Bool)

        F80.AST.CallExpr data ->
            let
                ctx_ =
                    updateCtxForPreludeCall data.fn ctx
            in
            checkCall ctx_ parentPath data

        F80.AST.IfExpr data ->
            checkExpr ctx parentPath data.cond type_
                |> Result.andThen
                    (\() ->
                        checkExpr ctx parentPath data.then_ type_
                            |> Result.andThen
                                (\() ->
                                    checkExpr ctx parentPath data.else_ type_
                                )
                    )


updateCtxForPreludeCall : String -> Ctx -> Ctx
updateCtxForPreludeCall fnName ctx =
    List.foldl
        (\( preludeName, preludeType ) accCtx ->
            if fnName == preludeName then
                accCtx
                    |> Dict.insert [ InDecl preludeName ] preludeType

            else
                accCtx
        )
        ctx
        prelude


checkCall : Ctx -> Path -> F80.AST.CallData -> Result Error ()
checkCall ctx parentPath data =
    case Dict.get [ InDecl data.fn ] ctx of
        Nothing ->
            Err { at = parentPath, type_ = FunctionNotFound { name = data.fn } }

        Just ((Function paramsType restType) as fnType) ->
            let
                expectedArity =
                    List.length paramsType

                actualArity =
                    List.length data.args
            in
            if expectedArity /= actualArity then
                Err
                    { at = parentPath
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
                                    case checkExpr ctx parentPath arg paramType of
                                        Ok () ->
                                            Ok ()

                                        Err err ->
                                            Err err
                        )
                        (Ok ())

        Just actual ->
            Err { at = parentPath, type_ = GlobalNotFunction { name = data.fn, actual = actual } }


checkEqual : Path -> Type -> Type -> Result Error ()
checkEqual parentPath expected actual =
    if expected == actual then
        Ok ()

    else
        Err { at = parentPath, type_ = TypeMismatch { expected = expected, actual = actual } }
