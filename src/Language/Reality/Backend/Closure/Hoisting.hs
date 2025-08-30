module Language.Reality.Backend.Closure.Hoisting where

import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | HOISTING
-- | Hoist all let-in expressions to the top level of the program.
-- | This is done to simplify the closure conversion process.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with lambda expressions hoisted.
hoistLambdas ::
    (MonadIO m) =>
    [HLIR.TLIR "toplevel"] ->
    m [HLIR.TLIR "toplevel"]
hoistLambdas toplevels = do
    hoisted <- mapM hoistLambdaSingular toplevels
    pure (concat hoisted)

-- | Hoist a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a list of toplevel
-- | nodes.
-- | This is used to hoist lambda expressions, as a lambda expression may
-- | resolve to multiple toplevel nodes.
hoistLambdaSingular ::
    (MonadIO m) =>
    HLIR.TLIR "toplevel" ->
    m [HLIR.TLIR "toplevel"]
hoistLambdaSingular (HLIR.MkTopLocated p e) = do
    hoisted <- hoistLambdaSingular e
    pure (map (HLIR.MkTopLocated p) hoisted)
hoistLambdaSingular (HLIR.MkTopFunctionDeclaration name params ret body) = do
    (newBody, hoisted) <- hoistLambdasInExpr body
    pure (hoisted ++ [HLIR.MkTopFunctionDeclaration name params ret newBody])
hoistLambdaSingular (HLIR.MkTopModuleDeclaration name body) = do
    hoistedBody <- hoistLambdas body
    pure [HLIR.MkTopModuleDeclaration name hoistedBody]
hoistLambdaSingular node = pure [node]

-- | Hoist lambda expressions in a HLIR expression.
-- | This function takes an expression, and returns a tuple of the new expression
-- | with lambda expressions hoisted, and a list of toplevel nodes that were hoisted.
hoistLambdasInExpr ::
    (MonadIO m) =>
    HLIR.TLIR "expression" ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"])
hoistLambdasInExpr (HLIR.MkExprLocated p e) = do
    (newExpr, hoisted) <- hoistLambdasInExpr e
    pure (HLIR.MkExprLocated p newExpr, hoisted)
hoistLambdasInExpr (HLIR.MkExprApplication callee args) = do
    (newCallee, hoistedCallee) <- hoistLambdasInExpr callee
    (newArgs, hoistedArgs) <- mapAndUnzipM hoistLambdasInExpr args
    pure
        (HLIR.MkExprApplication newCallee newArgs, hoistedCallee ++ concat hoistedArgs)
hoistLambdasInExpr (HLIR.MkExprVariable ann vars) =
    pure (HLIR.MkExprVariable ann vars, [])
hoistLambdasInExpr (HLIR.MkExprLiteral lit) =
    pure (HLIR.MkExprLiteral lit, [])
hoistLambdasInExpr (HLIR.MkExprLambda params ret body) = do
    (newBody, hoistedBody) <- hoistLambdasInExpr body
    lambdaName <- freshLambdaName

    let funcType = HLIR.MkTyFun (map (runIdentity <$> (.typeValue)) params) ret.runIdentity

    let lambdaToplevel =
            HLIR.MkTopFunctionDeclaration
                (HLIR.MkAnnotation lambdaName [])
                (map (runIdentity <$>) params)
                ret.runIdentity
                newBody
    pure
        ( HLIR.MkExprVariable (HLIR.MkAnnotation lambdaName (Identity funcType)) []
        , hoistedBody ++ [lambdaToplevel]
        )
hoistLambdasInExpr (HLIR.MkExprLetIn binding value inExpr ret) = do
    (newValue, hoistedValue) <- hoistLambdasInExpr value
    (newInExpr, hoistedInExpr) <- hoistLambdasInExpr inExpr
    pure
        (HLIR.MkExprLetIn binding newValue newInExpr ret, hoistedValue ++ hoistedInExpr)
hoistLambdasInExpr (HLIR.MkExprCondition cond thenB elseB branchType) = do
    (newCond, hoistedCond) <- hoistLambdasInExpr cond
    (newThenB, hoistedThenB) <- hoistLambdasInExpr thenB
    (newElseB, hoistedElseB) <- hoistLambdasInExpr elseB
    pure
        ( HLIR.MkExprCondition newCond newThenB newElseB branchType
        , hoistedCond ++ hoistedThenB ++ hoistedElseB
        )
hoistLambdasInExpr (HLIR.MkExprStructureAccess struct field) = do
    (newStruct, hoistedStruct) <- hoistLambdasInExpr struct
    pure (HLIR.MkExprStructureAccess newStruct field, hoistedStruct)
hoistLambdasInExpr (HLIR.MkExprStructureCreation ann fields) = do
    (newFields, hoistedFields) <- mapAndUnzipM hoistLambdasInExpr (Map.elems fields)

    let newFieldMap = Map.fromList $ zip (Map.keys fields) newFields
    pure (HLIR.MkExprStructureCreation ann newFieldMap, concat hoistedFields)
hoistLambdasInExpr (HLIR.MkExprDereference e targetType) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprDereference newE targetType, hoistedE)
hoistLambdasInExpr (HLIR.MkExprReference e targetType) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprReference newE targetType, hoistedE)
hoistLambdasInExpr (HLIR.MkExprUpdate update value updateType) = do
    (newUpdate, hoistedUpdate) <- hoistLambdasInExpr update
    (newValue, hoistedValue) <- hoistLambdasInExpr value

    pure (HLIR.MkExprUpdate newUpdate newValue updateType, hoistedUpdate ++ hoistedValue)
hoistLambdasInExpr (HLIR.MkExprSizeOf t) =
    pure (HLIR.MkExprSizeOf t, [])
hoistLambdasInExpr (HLIR.MkExprSingleIf cond thenB branchType) = do
    (newCond, hoistedCond) <- hoistLambdasInExpr cond
    (newThenB, hoistedThenB) <- hoistLambdasInExpr thenB
    pure (HLIR.MkExprSingleIf newCond newThenB branchType, hoistedCond ++ hoistedThenB)
hoistLambdasInExpr (HLIR.MkExprCast e t) = do
    (newE, hoistedE) <- hoistLambdasInExpr e
    pure (HLIR.MkExprCast newE t, hoistedE)

{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO (newIORef 0)

-- | Generate a unique name for a hoisted lambda function.
-- | This is done by incrementing a counter stored in an IORef.
-- | The generated name is of the form "__lambda_<counter>".
freshLambdaName :: (MonadIO m) => m Text
freshLambdaName = do
    modifyIORef' symbolCounter (+ 1)
    c <- liftIO $ readIORef symbolCounter
    pure $ "__lambda_" <> Text.pack (show c)
