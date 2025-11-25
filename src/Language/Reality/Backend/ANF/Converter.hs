module Language.Reality.Backend.ANF.Converter where

import Control.Monad.Result qualified as Err
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR
import Language.Reality.Syntax.MLIR qualified as MLIR
import qualified Data.List as List

isReturnExpr :: HLIR.TLIR "expression" -> Bool
isReturnExpr (HLIR.MkExprReturn _) = True
isReturnExpr (HLIR.MkExprLocated _ e) = isReturnExpr e
isReturnExpr _ = False

mapAndUnzip3M :: (Monad m) => (a -> m (b, c, d)) -> [a] -> m ([b], [c], [d])
mapAndUnzip3M f xs = do
    (bs, cs, ds) <- unzip3 <$> mapM f xs
    pure (bs, cs, ds)

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
convertToANF xs = do
    toplevels <- mapM convertToplevel xs
    pure ([MLIR.MkTopTypeAlias "never" (HLIR.MkTyId "void")] <> concat toplevels)

-- | Convert a HLIR toplevel to a MLIR toplevel.
-- | This function takes a HLIR toplevel, and returns a MLIR toplevel.
-- | The conversion is done by converting the body of the toplevel.
convertToplevel :: (MonadIO m) => HLIR.TLIR "toplevel" -> m [MLIR.Toplevel]
convertToplevel (HLIR.MkTopExternLet ann) = do
    pure [MLIR.MkTopExternalVariable ann.name ann.typeValue]
convertToplevel (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    (body', lets, _) <- convertExpression body

    -- If the function is "main", we need to add the GC start and end calls.
    -- We also need to add the "argc" and "argv" parameters if they are not present.
    -- We assume that "main" has the following signature:
    -- fn main(argc: int, argv: **char): int
    let gcStart =
            MLIR.MkExprApplication
                (MLIR.MkExprVariable "GC_INIT")
                [ ]

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
            | ann.name == "main" = gcStart : lets ++ [body']
            | otherwise = lets ++ [body']

    pure [MLIR.MkTopFunction ann.name args ret body'']
convertToplevel (HLIR.MkTopConstantDeclaration ann expr) = do
    (expr', lets, _) <- convertExpression expr

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
    pure [MLIR.MkTopStructure name.name fields]
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
                pure (HLIR.MkStructField (name <> "_field" <> Text.pack (show idx)) ty)
            pure $ HLIR.MkStructStruct name fieldNames

        let enumStructure =
                MLIR.MkTopStructure
                    enumName
                    [ HLIR.MkStructField "_tag" MLIR.MkTyString
                    , HLIR.MkStructUnion "_data" namedFields
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

isCFStatement :: HLIR.TLIR "expression" -> Bool
isCFStatement (HLIR.MkExprReturn _) = True
isCFStatement HLIR.MkExprBreak = True
isCFStatement HLIR.MkExprContinue = True
isCFStatement (HLIR.MkExprLocated _ e) = isCFStatement e
isCFStatement (HLIR.MkExprWhile _ _ _ inExpr) = isCFStatement inExpr
isCFStatement (HLIR.MkExprLetIn _ _ inExpr _) = isCFStatement inExpr
isCFStatement (HLIR.MkExprCondition _ thenB elseB _) =
    isCFStatement thenB || isCFStatement elseB
isCFStatement (HLIR.MkExprSingleIf _ thenB _) = isCFStatement thenB
isCFStatement _ = False

getIs :: HLIR.TLIR "expression" -> Maybe (HLIR.TLIR "expression", HLIR.TLIR "pattern", HLIR.Type)
getIs (HLIR.MkExprIs expr pat exprTy) = Just (expr, pat, exprTy.runIdentity)
getIs (HLIR.MkExprLocated _ e) = getIs e
getIs _ = Nothing

isVariable :: HLIR.TLIR "expression" -> Text -> Bool
isVariable (HLIR.MkExprVariable ann _) name = ann.name == name
isVariable (HLIR.MkExprLocated _ e) name = isVariable e name
isVariable _ _ = False

getConditions :: HLIR.TLIR "expression" -> [HLIR.TLIR "expression"]
getConditions (HLIR.MkExprCondition cond thenB elseB _) =
    cond : (getConditions thenB ++ getConditions elseB)
getConditions (HLIR.MkExprLocated _ e) = getConditions e
getConditions (HLIR.MkExprApplication callee args t)
    | isVariable callee "and" = do
        let conds = concatMap getConditions args
        conds
    | otherwise = 
        [HLIR.MkExprApplication 
            callee 
            (concatMap getConditions args)
            t]
getConditions e = [e]

createCondition ::
    MonadIO m =>
    [HLIR.TLIR "expression"] ->
    (MLIR.Expression, [MLIR.Expression], [MLIR.Expression]) ->
    (MLIR.Expression, [MLIR.Expression], [MLIR.Expression]) ->
    Bool ->
    Text ->
    m [MLIR.Expression]
createCondition [] (thenB, l1, bs2) _ shouldUpdate n
    | not shouldUpdate && n /= "" = do
        let update = MLIR.MkExprUpdate (MLIR.MkExprVariable n) thenB
            in pure $ l1 ++ bs2 ++ [update]
    | otherwise = pure $ l1 ++ bs2 ++ [thenB]
createCondition (cond' : rest) t e@(elseB, l3, bs3) su n = do
    (cond'', l1, bs1) <- convertExpression cond'

    innerCondition <- createCondition rest t e su n
    let elseExpr
            | not su && n /= "" = 
                let update = MLIR.MkExprUpdate (MLIR.MkExprVariable n) elseB
                in MLIR.MkExprBlock $ l3 ++ bs3 ++ [update]
            | otherwise = MLIR.MkExprBlock $ l3 ++ bs3 ++ [elseB]
        ifExpr = l1 ++ bs1 ++ [MLIR.MkExprCondition cond'' (MLIR.MkExprBlock innerCondition) elseExpr]
    
    pure ifExpr

isVoidType :: MonadIO m => HLIR.Type -> m Bool
isVoidType (HLIR.MkTyId "never") = pure True
isVoidType (HLIR.MkTyFun _ retType) = isVoidType retType
isVoidType (HLIR.MkTyVar ref) = do
    ty <- liftIO $ readIORef ref
    case ty of
        HLIR.Link ty' -> isVoidType ty'
        HLIR.Unbound{} -> pure False
isVoidType _ = pure False

-- | Convert a HLIR expression to a MLIR expression in ANF.
-- | This function takes a HLIR expression, and returns a MLIR expression.
convertExpression ::
    (MonadIO m) => HLIR.TLIR "expression" -> m (MLIR.Expression, [MLIR.Expression], [MLIR.Expression])
convertExpression (HLIR.MkExprVariable ann _) = do
    isVoid <- isVoidType ann.typeValue.runIdentity

    if isVoid then
        pure (MLIR.MkExprSpecialVariable ann.name, [], [])
    else
        pure (MLIR.MkExprVariable ann.name, [], [])
convertExpression (HLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [], [])
convertExpression (HLIR.MkExprApplication f args _) = do
    (f', l1, bs1) <- convertExpression f
    (args', l2s, bs2) <- unzip3 <$> mapM convertExpression args

    let app = MLIR.MkExprApplication f' args'

    pure (app, l1 ++ concat l2s, bs1 ++ concat bs2)

-- If encountering empty let-binding (used for sequencing), ignore the name
-- and just convert the value and inExpr.
convertExpression (HLIR.MkExprLetIn (HLIR.MkAnnotation "_" _) value inExpr _) = do
    (value', l1, bs1) <- convertExpression value
    (inExpr', l2, bs2) <- convertExpression inExpr

    pure (inExpr', l1 ++ [value'] ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprLetIn binding value inExpr _) = do
    (value', l1, bs1) <- convertExpression value
    (inExpr', l2, bs2) <- convertExpression inExpr

    -- If the inExpr is a control flow statement (return, break, continue),
    -- we don't need to create a new variable for it, as it will not be used.
    let letBinding = MLIR.MkExprLet binding.name binding.typeValue.runIdentity (Just value')
    pure (inExpr', l1 ++ [letBinding] ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprCondition cond thenB elseB branchTy) = do
    let conditions = getConditions cond

    -- Converting the condition, then branch and else branch.
    then' <- convertExpression thenB
    else' <- convertExpression elseB

    let isCF = isCFStatement thenB || isCFStatement elseB
    isVoid <- isVoidType branchTy.runIdentity

    if isCF || isVoid then do
        expr <- createCondition conditions then' else' isCF ""
        -- If one of the branches is a control flow statement,
        -- we don't need to create a new variable for it, as it will not be used.

        pure (MLIR.MkExprVariable "", expr, [])
    else do
        -- Creating a fresh symbol for storing the result of the condition.
        newVariable <- freshSymbol

        -- Converting condition can be tricky because both branches need
        -- to include lets-generated variables and update the same variable.
        --
        -- This permits to have conditions as expressions as well as conditions
        -- as statements.

        let def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing
        ifExpr <- createCondition conditions then' else' isCF newVariable

        pure (MLIR.MkExprVariable newVariable, [def] <> ifExpr, [])
convertExpression (HLIR.MkExprLocated _ e) = convertExpression e
convertExpression (HLIR.MkExprStructureAccess struct field) = do
    (struct', l1, bs1) <- convertExpression struct
    let access = MLIR.MkExprStructureAccess struct' field
    pure (access, l1, bs1)
convertExpression (HLIR.MkExprStructureCreation ann fields) = do
    (fieldExprs, l2s, bs2s) <- mapAndUnzip3M convertExpression (Map.elems fields)

    let fieldNames = Map.keys fields

    newVariable <- freshSymbol

    let structCreation = MLIR.MkExprStructureCreation ann (Map.fromList (zip fieldNames fieldExprs))
        letBinding = MLIR.MkExprLet newVariable ann (Just structCreation)

    pure (MLIR.MkExprVariable newVariable, concat l2s ++ [letBinding], mconcat bs2s)
convertExpression (HLIR.MkExprDereference e _) = do
    (e', l1, bs1) <- convertExpression e
    pure (MLIR.MkExprDereference e', l1, bs1)
convertExpression (HLIR.MkExprReference e ret) = do
    -- Creating a reference requires creating a let-binding for the inner expression,
    (e', l1, bs1) <- convertExpression e

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

    pure (MLIR.MkExprVariable newRefVariable, l1 ++ [letBinding, refBinding], bs1)
convertExpression (HLIR.MkExprUpdate (HLIR.MkExprApplication f args ty) value _) = do
    (app', l1, bs1) <- convertExpression (HLIR.MkExprReference (HLIR.MkExprApplication f args ty) (Identity (MLIR.MkTyPointer ty.runIdentity)))
    (value', l3, bs3) <- convertExpression value

    name <- freshSymbol

    let update_let = MLIR.MkExprLet name (MLIR.MkTyPointer ty.runIdentity) (Just app')
        update = MLIR.MkExprUpdate (MLIR.MkExprDereference (MLIR.MkExprVariable name)) value'

    pure (update, l1 ++ [update_let] ++ l3, bs1 ++ bs3)
convertExpression (HLIR.MkExprUpdate (HLIR.MkExprDereference e target) value _) = do
    (e', l1, bs1) <- convertExpression e
    (value', l2, bs2) <- convertExpression value

    newName <- freshSymbol

    -- We don't create early let-binding for *<e> because
    -- it would create a temporary variable that would
    -- disallow updates on pointers.
    let deref = MLIR.MkExprDereference (MLIR.MkExprVariable newName)
        update = MLIR.MkExprUpdate deref value'
        letExpr = MLIR.MkExprLet newName (MLIR.MkTyPointer target.runIdentity) (Just e')

    pure (update, l1 ++ [letExpr] ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprSingleIf cond thenB branchTy) = do
    let conds = getConditions cond
    then' <- convertExpression thenB

    let isCF = isCFStatement thenB
    isVoid <- isVoidType branchTy.runIdentity

    if isCF || isVoid then do
        ifExpr <- createCondition conds then' (MLIR.MkExprBlock [], [], []) isCF ""

        pure (MLIR.MkExprVariable "", ifExpr, [])
    else do
        newVariable <- freshSymbol
        let var = MLIR.MkExprVariable newVariable
            def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing

        ifExpr <- createCondition conds then' (var, [], []) isCF newVariable
        pure (MLIR.MkExprVariable newVariable, def : ifExpr, [])
convertExpression (HLIR.MkExprUpdate update value _) = do
    (update', l1, bs1) <- convertExpression update
    (value', l2, bs2) <- convertExpression value
    pure (MLIR.MkExprUpdate update' value', l1 ++ l2, bs1 ++ bs2)
convertExpression (HLIR.MkExprSizeOf t) = pure (MLIR.MkExprSizeOf t, [], [])
convertExpression (HLIR.MkExprLambda{}) =
    Err.compilerError
        "Lambdas should have been converted to top-level functions before ANF conversion."
convertExpression (HLIR.MkExprCast e t) = do
    (e', l1, bs1) <- convertExpression e
    pure (MLIR.MkExprCast t e', l1, bs1)
convertExpression (HLIR.MkExprWhile cond body _ inExpr) = do
    (cond', l1, bs1) <- convertExpression cond
    (body', l2, bs2) <- convertExpression body
    (inExpr', l3, bs3) <- convertExpression inExpr

    let bl = MLIR.MkExprBlock (l2 ++ [body'])
        whileExpr = MLIR.MkExprWhile cond' bl

    pure (inExpr', l1 ++ [whileExpr] ++ l3, bs1 ++ bs2 ++ bs3)
convertExpression (HLIR.MkExprIs e p ty) = do
    (e', l1, bs1) <- convertExpression e

    newName <- freshSymbol
    let nameBinding = MLIR.MkExprLet newName ty.runIdentity (Just e')
        nameExpr = MLIR.MkExprVariable newName

    (lets, conds) <- generateCondition nameExpr p

    let lets' = [MLIR.MkExprLet name ty' (Just expr) | (name, ty', expr) <- lets, ty' /= MLIR.MkTyId "void"]

    let cond =
            List.foldr
                (\a b -> MLIR.MkExprApplication (MLIR.MkExprVariable "&&") [a, b])
                (MLIR.MkExprLiteral (MLIR.MkLitBool True))
                conds

    pure (cond, l1, [nameBinding] <> lets' <> bs1)
convertExpression (HLIR.MkExprFunctionAccess{}) =
    Err.compilerError
        "Method calls should have been converted to function calls before ANF conversion."
convertExpression (HLIR.MkExprReturn e) = do
    (e', l1, bs1) <- convertExpression e
    pure (MLIR.MkExprReturn e', l1, bs1)
convertExpression HLIR.MkExprBreak = pure (MLIR.MkExprBreak, [], [])
convertExpression HLIR.MkExprContinue = pure (MLIR.MkExprContinue, [], [])
convertExpression (HLIR.MkExprLetPatternIn {}) = 
    Err.compilerError
        "Let-pattern expressions should have been desugared before ANF conversion."

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
