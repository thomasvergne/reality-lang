module Language.Reality.Backend.ANF.Converter where

import Language.Reality.Syntax.HLIR qualified as HLIR
import Language.Reality.Syntax.MLIR qualified as MLIR
import Control.Monad.Result qualified as Err
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO

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
    MonadIO m =>
    [HLIR.TLIR "toplevel"] ->
    m [MLIR.Toplevel]
convertToANF = mapM convertToplevel

-- | Convert a HLIR toplevel to a MLIR toplevel.
-- | This function takes a HLIR toplevel, and returns a MLIR toplevel.
-- | The conversion is done by converting the body of the toplevel.
convertToplevel :: MonadIO m => HLIR.TLIR "toplevel" -> m MLIR.Toplevel
convertToplevel (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    (body', lets) <- convertExpression body

    let gcStart = MLIR.MkExprApplication (MLIR.MkExprVariable "gc_start") [
              MLIR.MkExprApplication (MLIR.MkExprVariable "get_gc") []
            , MLIR.MkExprReference (MLIR.MkExprVariable "argc")
            ]

        gcEnd = MLIR.MkExprApplication (MLIR.MkExprVariable "gc_stop") [
            MLIR.MkExprApplication (MLIR.MkExprVariable "get_gc") []
            ]

    let args
            | ann.name == "main" && length params /= 2 =
                [ MLIR.MkAnnotation "argc" MLIR.MkTyInt
                , MLIR.MkAnnotation "argv" (MLIR.MkTyPointer (MLIR.MkTyPointer MLIR.MkTyChar))
                ]
            | otherwise = params


    let body''
            | ann.name == "main" = gcStart : lets ++ [gcEnd, body']
            | otherwise = lets ++ [body']

    pure $ MLIR.MkTopFunction ann.name args ret body''
convertToplevel (HLIR.MkTopConstantDeclaration ann expr) = do
    (expr', lets) <- convertExpression expr

    unless (null lets) $
        Err.compilerError
            "Top-level constant declarations cannot contain let bindings. Please inline the expression."

    pure $ MLIR.MkTopGlobal ann.name ann.typeValue (Just expr')
convertToplevel (HLIR.MkTopLocated _ e) = convertToplevel e
convertToplevel (HLIR.MkTopModuleDeclaration {}) =
    Err.compilerError "Modules should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopImport _) =
    Err.compilerError "Imports should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopStructureDeclaration name fields) =
    pure $ MLIR.MkTopStructure name.name (Map.toList fields)
convertToplevel (HLIR.MkTopTypeAlias {}) =
    Err.compilerError "Type aliases should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopPublic node) = do
    node' <- convertToplevel node
    pure $ MLIR.MkTopPublic node'
convertToplevel (HLIR.MkTopExternalFunction name parameters returnType) =
    pure $ MLIR.MkTopExternalFunction name.name name.typeValue (map (.typeValue) parameters) returnType
convertToplevel (HLIR.MkTopImplementation {}) =
    Err.compilerError "Implementations should have been resolved before ANF conversion."
convertToplevel (HLIR.MkTopProperty {}) =
    Err.compilerError "Properties should have been resolved before ANF conversion."

-- | Convert a HLIR expression to a MLIR expression in ANF.
-- | This function takes a HLIR expression, and returns a MLIR expression.
convertExpression :: MonadIO m => HLIR.TLIR "expression" -> m (MLIR.Expression, [MLIR.Expression])
convertExpression (HLIR.MkExprLiteral l) = pure (MLIR.MkExprLiteral l, [])
convertExpression (HLIR.MkExprVariable ann _) = pure (MLIR.MkExprVariable ann.name, [])
convertExpression (HLIR.MkExprApplication f args) = do
    (f', l1) <- convertExpression f
    (args', l2s) <- mapAndUnzipM convertExpression args

    let app = MLIR.MkExprApplication f' args'

    pure (app, l1 ++ concat l2s)
convertExpression (HLIR.MkExprLetIn (HLIR.MkAnnotation "_" _) value inExpr ret) = do
    (value', l1) <- convertExpression value
    (inExpr', l2) <- convertExpression inExpr

    newVariable <- freshSymbol

    let newLet = MLIR.MkExprLet newVariable ret.runIdentity (Just inExpr')

    pure (MLIR.MkExprVariable newVariable, l1 ++ [value'] ++ l2 ++ [newLet])
convertExpression (HLIR.MkExprLetIn binding value inExpr ret) = do
    (value', l1) <- convertExpression value
    (inExpr', l2) <- convertExpression inExpr

    newVariable <- freshSymbol

    let newLet = MLIR.MkExprLet newVariable ret.runIdentity (Just inExpr')
        letBinding = MLIR.MkExprLet binding.name binding.typeValue.runIdentity (Just value')

    pure (MLIR.MkExprVariable newVariable, l1 ++ [letBinding] ++ l2 ++ [newLet])
convertExpression (HLIR.MkExprCondition cond thenB elseB branchTy) = do
    (cond', l1) <- convertExpression cond
    (thenB', l2) <- convertExpression thenB
    (elseB', l3) <- convertExpression elseB

    newVariable <- freshSymbol
    let var = MLIR.MkExprVariable newVariable

    let bl1 = MLIR.MkExprBlock (l2 ++ [MLIR.MkExprUpdate var thenB'])
    let bl2 = MLIR.MkExprBlock (l3 ++ [MLIR.MkExprUpdate var elseB'])

    let def = MLIR.MkExprLet newVariable branchTy.runIdentity Nothing

    let ifExpr = MLIR.MkExprCondition cond' bl1 bl2
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
    (e', l1) <- convertExpression e
    newVariable <- freshSymbol
    newRefVariable <- freshSymbol

    let ty = case ret.runIdentity of
            MLIR.MkTyPointer innerTy -> innerTy
            retTy -> retTy

    let ref = MLIR.MkExprReference (MLIR.MkExprVariable newVariable)
        letBinding = MLIR.MkExprLet newVariable ty (Just e')
        refBinding = MLIR.MkExprLet newRefVariable (MLIR.MkTyPointer ty) (Just ref)
    pure (MLIR.MkExprVariable newRefVariable, l1 ++ [letBinding, refBinding])
convertExpression (HLIR.MkExprUpdate (HLIR.MkExprDereference e target) value _) = do
    (e', l1) <- convertExpression e
    (value', l2) <- convertExpression value

    newName <- freshSymbol

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
convertExpression (HLIR.MkExprLambda {}) =
    Err.compilerError "Lambdas should have been converted to top-level functions before ANF conversion."
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
    pure (var, l1 ++ l3 ++ [def, whileExpr])


{-# NOINLINE symbolCounter #-}
symbolCounter :: IORef Int
symbolCounter = IO.unsafePerformIO (newIORef 0)

freshSymbol :: MonadIO m => m Text
freshSymbol = do
    modifyIORef' symbolCounter (+1)
    i <- readIORef symbolCounter
    pure $ "anf_tmp_" <> Text.pack (show i)
