module Language.Reality.Backend.ANF.Converter where

import Control.Monad.Result qualified as Err
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR
import Language.Reality.Syntax.MLIR qualified as MLIR

-- | ANF CONVERTER
-- | Convert a HLIR expression to ANF (MLIR).
-- | This function takes a HLIR expression, and returns a MLIR expression.
-- | The conversion is done by introducing let bindings for non-trivial expressions.
-- | Trivial expressions are:
-- | - Variables
-- | - Constants
-- | - Function applications
-- | - Structure access
-- | - SizeOf
-- | Non-trivial expressions are:
-- | - Let expressions
-- | - Conditionals
-- | - Structure creation
-- | - Dereference
-- | - Reference
-- | - Update
-- | The conversion is done recursively, so that all non-trivial expressions are
-- | converted to let bindings.
convertToANF ::
    (MonadIO m) =>
    [HLIR.TLIR "toplevel"] ->
    m [MLIR.Toplevel]
convertToANF = (concat <$>) . mapM convertToplevel

-- | Convert a HLIR toplevel to a MLIR toplevel.
-- | This function takes a HLIR toplevel, and returns a MLIR toplevel.
-- | The conversion is done by converting the body of the toplevel.
convertToplevel :: (MonadIO m) => HLIR.TLIR "toplevel" -> m [MLIR.Toplevel]
convertToplevel (HLIR.MkTopExternLet ann) = do
    pure [MLIR.MkTopExternalVariable ann.name ann.typeValue]
convertToplevel (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    (body', lets) <- convertExpression body

    -- If the function is "main", we need to add the GC start and end calls.
    -- We also need to add the "argc" and "argv" parameters if they are not present.
    -- We assume that "main" has the following signature:
    -- fn main(argc: int, argv: **char): int
    let gcStart =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "gc_start")
                [ MLIR.MkExprApplication (MLIR.MkExprVariable "get_gc") []
                , MLIR.MkExprReference (MLIR.MkExprVariable "argc")
                ]

        gcEnd =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "gc_stop")
                [ MLIR.MkExprApplication (MLIR.MkExprVariable "get_gc") []
                ]

    -- Resetting the main arguments to the expected ones if they are not correct.
    let args
            | ann.name == "main" && length params /= 2 =
                [ MLIR.MkAnnotation "argc" MLIR.MkTyInt
                , MLIR.MkAnnotation "argv" (MLIR.MkTyPointer (MLIR.MkTyPointer MLIR.MkTyChar))
                ]
            | otherwise = params

    -- Building the final body with GC calls if needed.
    -- If the function is not "main", we just use the body with let bindings.
    let body''
            | ann.name == "main" = gcStart : lets ++ [gcEnd, body']
            | otherwise = lets ++ [body']

    pure [MLIR.MkTopFunction ann.name args ret body'']
convertToplevel (HLIR.MkTopConstantDeclaration ann expr) = do
    (expr', lets) <- convertExpression expr

    unless (null lets)
        $ Err.compilerError
            "Top-level constant declarations cannot contain let bindings. Please inline the expression."

    pure [MLIR.MkTopGlobal ann.name ann.typeValue (Just expr')]
convertToplevel (HLIR.MkTopLocated _ e) = convertToplevel e
convertToplevel (HLIR.MkTopModuleDeclaration{}) =
    Err.compilerError "Modules should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopImport _) =
    Err.compilerError "Imports should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopStructureDeclaration name fields) = do
    let fields' = map (uncurry MLIR.MkStructField) (Map.toList fields)
    pure [MLIR.MkTopStructure name.name fields']
convertToplevel (HLIR.MkTopTypeAlias{}) =
    Err.compilerError
        "Type aliases should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopPublic node) = do
    node' <- convertToplevel node
    pure $ MLIR.MkTopPublic <$> node'
convertToplevel (HLIR.MkTopExternalFunction name parameters returnType) =
    pure
        [ MLIR.MkTopExternalFunction
            name.name
            name.typeValue
            (map (.typeValue) parameters)
            returnType
        ]
convertToplevel (HLIR.MkTopImplementation{}) =
    Err.compilerError
        "Implementations should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopProperty{}) =
    Err.compilerError "Properties should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopAnnotation{}) =
    Err.compilerError "Annotations should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopEnumeration header constructors) = do
    enum <- createEnumStructure (header.name, constructors)
    functions <- mapM (createFunction header.name) (Map.toList constructors)

    pure (enum : functions)
  where
    createEnumStructure ::
        (MonadIO m) => (Text, Map Text (Maybe [MLIR.Type])) -> m MLIR.Toplevel
    createEnumStructure (enumName, constructors') = do
        let fields = Map.map (fromMaybe []) constructors'

        namedFields <- forM (Map.toList fields) $ \(name, fieldTypes) -> do
            fieldNames <- forM (zip fieldTypes [(1 :: Int) ..]) $ \(ty, idx) ->
                pure (MLIR.MkStructField (name <> "_field" <> Text.pack (show idx)) ty)
            pure $ MLIR.MkStructStruct name fieldNames

        let enumStructure =
                MLIR.MkTopStructure
                    enumName
                    [ MLIR.MkStructField "_tag" MLIR.MkTyString
                    , MLIR.MkStructUnion "_data" namedFields
                    ]

        pure enumStructure

    createFunction ::
        (MonadIO m) => Text -> (Text, Maybe [MLIR.Type]) -> m MLIR.Toplevel
    createFunction enumName (consName, maybeFieldTypes) = do
        let arguments = case maybeFieldTypes of
                Just fieldTypes ->
                    zipWith
                        (\ty idx -> MLIR.MkAnnotation ("arg" <> Text.pack (show idx)) ty)
                        fieldTypes
                        [(1 :: Int) ..]
                Nothing -> []

            structType = MLIR.MkTyId enumName

            bodyExpr =
                [ MLIR.MkExprLet "result" structType Nothing
                , MLIR.MkExprUpdate
                    (MLIR.MkExprStructureAccess (MLIR.MkExprVariable "result") "_tag")
                    (MLIR.MkExprLiteral (MLIR.MkLitString consName))
                ]
                    <> [ MLIR.MkExprUpdate
                            ( MLIR.MkExprStructureAccess
                                ( MLIR.MkExprStructureAccess
                                    (MLIR.MkExprStructureAccess (MLIR.MkExprVariable "result") "_data")
                                    consName
                                )
                                (consName <> "_field" <> Text.pack (show idx))
                            )
                            (MLIR.MkExprVariable arg.name)
                       | (arg, idx) <- zip arguments [(1 :: Int) ..]
                       ]
                    ++ [MLIR.MkExprVariable "result"]

        pure
            $ MLIR.MkTopFunction
                consName
                arguments
                structType
                bodyExpr

-- | Convert a HLIR expression to a MLIR expression in ANF.
-- | This function takes a HLIR expression, and returns a MLIR expression.
convertExpression ::
    (MonadIO m) => HLIR.TLIR "expression" -> m (MLIR.Expression, [MLIR.Expression])
convertExpression (HLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [])
convertExpression (HLIR.MkExprVariable ann _) = pure (MLIR.MkExprVariable ann.name, [])
convertExpression (HLIR.MkExprApplication f args _) = do
    (f', l1) <- convertExpression f
    (args', l2s) <- mapAndUnzipM convertExpression args

    let app = MLIR.MkExprApplication f' args'

    pure (app, l1 ++ concat l2s)

-- If encountering empty let-binding (used for sequencing), ignore the name
-- and just convert the value and inExpr.
convertExpression (HLIR.MkExprLetIn (HLIR.MkAnnotation "_" _) value inExpr ret) = do
    (value', l1) <- convertExpression value
    (inExpr', l2) <- convertExpression inExpr

    newVariable <- freshSymbol

    let newLet = MLIR.MkExprLet newVariable ret.runIdentity (Just inExpr')

    pure (MLIR.MkExprVariable newVariable, l1 ++ [value'] ++ l2 ++ [newLet])
convertExpression (HLIR.MkExprLetIn binding value inExpr ret) = do
    (value', l1) <- convertExpression value
    (inExpr', l2) <- convertExpression inExpr

    -- Creating a fresh symbol for storing the result of the inExpr.
    -- This is necessary to ensure that the variable name is unique
    -- and does not clash with any other variable names in the scope.
    newVariable <- freshSymbol

    -- Creating the let bindings.
    -- The first let binding is for the inExpr, which binds the result
    -- of the inExpr to the new variable.
    -- The second let binding is for the original binding, which binds
    -- the value to the original variable name.
    let newLet = MLIR.MkExprLet newVariable ret.runIdentity (Just inExpr')
        letBinding = MLIR.MkExprLet binding.name binding.typeValue.runIdentity (Just value')

    -- Returning the new variable and the list of let bindings.
    -- The order of the let bindings is important, as the inExpr
    -- must be evaluated after the value.
    pure (MLIR.MkExprVariable newVariable, l1 ++ [letBinding] ++ l2 ++ [newLet])
convertExpression (HLIR.MkExprCondition cond thenB elseB branchTy) = do
    -- Converting the condition, then branch and else branch.
    (cond', l1) <- convertExpression cond
    (thenB', l2) <- convertExpression thenB
    (elseB', l3) <- convertExpression elseB

    -- Creating a fresh symbol for storing the result of the condition.
    newVariable <- freshSymbol
    let var = MLIR.MkExprVariable newVariable

    -- Converting condition can be tricky because both branches need
    -- to include lets-generated variables and update the same variable.
    --
    -- This permits to have conditions as expressions as well as conditions
    -- as statements.
    let bl1 = MLIR.MkExprBlock (l2 ++ [MLIR.MkExprUpdate var thenB'])
    let bl2 = MLIR.MkExprBlock (l3 ++ [MLIR.MkExprUpdate var elseB'])

    let def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing
        ifExpr = MLIR.MkExprCondition cond' bl1 bl2

    pure (MLIR.MkExprVariable newVariable, l1 ++ [def, ifExpr])
convertExpression (HLIR.MkExprLocated _ e) = convertExpression e
convertExpression (HLIR.MkExprStructureAccess struct field) = do
    (struct', l1) <- convertExpression struct
    let access = MLIR.MkExprStructureAccess struct' field
    pure (access, l1)
convertExpression (HLIR.MkExprStructureCreation ann fields) = do
    (fieldExprs, l2s) <- mapAndUnzipM convertExpression (Map.elems fields)
    let fieldNames = Map.keys fields
    newVariable <- freshSymbol
    let structCreation = MLIR.MkExprStructureCreation ann (Map.fromList (zip fieldNames fieldExprs))
        letBinding = MLIR.MkExprLet newVariable ann (Just structCreation)
    pure (MLIR.MkExprVariable newVariable, concat l2s ++ [letBinding])
convertExpression (HLIR.MkExprDereference e _) = do
    (e', l1) <- convertExpression e
    pure (MLIR.MkExprDereference e', l1)
convertExpression (HLIR.MkExprReference e ret) = do
    -- Creating a reference requires creating a let-binding for the inner expression,
    (e', l1) <- convertExpression e

    newVariable <- freshSymbol
    newRefVariable <- freshSymbol

    let ty = case ret.runIdentity of
            MLIR.MkTyPointer innerTy -> innerTy
            retTy -> retTy

    -- Creating the let bindings.
    -- It follows the following rules:
    --
    -- let <newVariable>: <ty> = <e'>;
    -- let <newRefVariable>: pointer<ty> = &<newVariable>;
    -- <newRefVariable>
    let ref = MLIR.MkExprReference (MLIR.MkExprVariable newVariable)
        letBinding = MLIR.MkExprLet newVariable ty (Just e')
        refBinding = MLIR.MkExprLet newRefVariable (MLIR.MkTyPointer ty) (Just ref)

    pure (MLIR.MkExprVariable newRefVariable, l1 ++ [letBinding, refBinding])
convertExpression (HLIR.MkExprUpdate (HLIR.MkExprDereference e target) value _) = do
    (e', l1) <- convertExpression e
    (value', l2) <- convertExpression value

    newName <- freshSymbol

    -- We don't create early let-binding for *<e> because
    -- it would create a temporary variable that would
    -- disallow updates on pointers.
    let deref = MLIR.MkExprDereference (MLIR.MkExprVariable newName)
        update = MLIR.MkExprUpdate deref value'
        letExpr = MLIR.MkExprLet newName (MLIR.MkTyPointer target.runIdentity) (Just e')

    pure (update, l1 ++ [letExpr] ++ l2)
convertExpression (HLIR.MkExprSingleIf cond thenB branchTy) = do
    (cond', l1) <- convertExpression cond
    (thenB', l2) <- convertExpression thenB

    newVariable <- freshSymbol
    let var = MLIR.MkExprVariable newVariable

    let bl1 = MLIR.MkExprBlock (l2 ++ [MLIR.MkExprUpdate var thenB'])

    let def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing

    let ifExpr = MLIR.MkExprSingleIf cond' bl1
    pure (MLIR.MkExprVariable newVariable, l1 ++ [def, ifExpr])
convertExpression (HLIR.MkExprUpdate update value _) = do
    (update', l1) <- convertExpression update
    (value', l2) <- convertExpression value
    pure (MLIR.MkExprUpdate update' value', l1 ++ l2)
convertExpression (HLIR.MkExprSizeOf t) = pure (MLIR.MkExprSizeOf t, [])
convertExpression (HLIR.MkExprLambda{}) =
    Err.compilerError
        "Lambdas should have been converted to top-level functions before ANF conversion."
convertExpression (HLIR.MkExprCast e t) = do
    (e', l1) <- convertExpression e
    pure (MLIR.MkExprCast t e', l1)
convertExpression (HLIR.MkExprWhile cond body inTy inExpr) = do
    (cond', l1) <- convertExpression cond
    (body', l2) <- convertExpression body
    (inExpr', l3) <- convertExpression inExpr

    newVariable <- freshSymbol
    let var = MLIR.MkExprVariable newVariable
    let bl = MLIR.MkExprBlock (l2 ++ [body'])
    let def = MLIR.MkExprLet newVariable inTy.runIdentity (Just inExpr')
    let whileExpr = MLIR.MkExprWhile cond' bl
    pure (var, l1 ++ [whileExpr] ++ l3 ++ [def])
convertExpression (HLIR.MkExprIfIs expr pat thenB elseB branchTy) = do
    (expr', l1) <- convertExpression expr
    (thenB', l2) <- convertExpression thenB
    result <- maybe (pure Nothing) ((pure <$>) . convertExpression) elseB

    newVariable <- freshSymbol
    let var = MLIR.MkExprVariable newVariable
    let def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing

    (lets, conds) <- generateCondition expr' pat

    let cond =
            List.foldr1
                (\a b -> MLIR.MkExprApplication (MLIR.MkExprVariable "&&") [a, b])
                conds

    let bl1 =
            MLIR.MkExprBlock
                ( map (\(name, ty, v) -> MLIR.MkExprLet name ty (Just v)) lets
                    ++ l2
                    ++ [MLIR.MkExprUpdate var thenB']
                )

    let ifExpr = MLIR.MkExprCondition cond bl1 $ case result of
            Just (elseB', l3) -> MLIR.MkExprBlock (l3 ++ [MLIR.MkExprUpdate var elseB'])
            Nothing -> MLIR.MkExprBlock []

    pure (var, l1 ++ [def, ifExpr])

type Lets = [(Text, MLIR.Type, MLIR.Expression)]
type Conditions = [MLIR.Expression]

generateCondition ::
    (MonadIO m) =>
    MLIR.Expression ->
    HLIR.TLIR "pattern" ->
    m (Lets, Conditions)
generateCondition e (HLIR.MkPatternLocated _ node) = generateCondition e node
generateCondition e (HLIR.MkPatternLet ann) = do
    pure ([(ann.name, ann.typeValue.runIdentity, e)], [])
generateCondition e (HLIR.MkPatternVariable ann) = do
    let tag = MLIR.MkExprStructureAccess e "_tag"
        cond =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "string_eq")
                [tag, MLIR.MkExprLiteral (MLIR.MkLitString ann.name)]

    pure ([], [cond])
generateCondition e (HLIR.MkPatternConstructor name patterns _) = do
    let tag = MLIR.MkExprStructureAccess e "_tag"
        cond =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "string_eq")
                [tag, MLIR.MkExprLiteral (MLIR.MkLitString name)]

    (patternsLets, patternsConds) <-
        mapAndUnzipM
            ( \(p, idx) ->
                generateCondition
                    ( MLIR.MkExprStructureAccess
                        (MLIR.MkExprStructureAccess (MLIR.MkExprStructureAccess e "_data") name)
                        (name <> "_field" <> show idx)
                    )
                    p
            )
            (zip patterns [(1 :: Int) ..])

    pure (concat patternsLets, cond : concat patternsConds)
generateCondition _ HLIR.MkPatternWildcard = pure ([], [])
generateCondition x (HLIR.MkPatternLiteral l) = do
    let fun = case l of
            HLIR.MkLitInt{} -> "=="
            HLIR.MkLitChar{} -> "=="
            HLIR.MkLitString{} -> "str_equal"
            HLIR.MkLitBool{} -> "=="
            HLIR.MkLitFloat{} -> "=="

        cond =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable fun)
                [x, MLIR.MkExprLiteral l]
    pure ([], [cond])
generateCondition x (HLIR.MkPatternStructure _ fields) = do
    (fieldsLets, fieldsConds) <-
        mapAndUnzipM
            (\(name, p) -> generateCondition (MLIR.MkExprStructureAccess x name) p)
            (Map.toList fields)
    pure (concat fieldsLets, concat fieldsConds)

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO (newIORef 0)

freshSymbol :: (MonadIO m) => m Text
freshSymbol = do
    modifyIORef' symbolCounter (+ 1)
    i <- readIORef symbolCounter
    pure $ "anf_tmp_" <> Text.pack (show i)
