module Language.Reality.Backend.Closure.Free where

import Data.Map qualified as Map
import Language.Reality.Syntax.HLIR qualified as HLIR

class Free a where
    free :: a -> Map Text HLIR.Type

instance (Free a) => Free [a] where
    free = foldMap free

instance (Free a) => Free (Maybe a) where
    free = foldMap free

instance (Free a, Free b) => Free (a, b) where
    free (a, b) = free a <> free b

instance (Free a, Free b, Free c) => Free (a, b, c) where
    free (a, b, c) = free a <> free b <> free c

instance Free (HLIR.Expression Identity HLIR.Type) where
    free (HLIR.MkExprApplication f args _) = free f <> free args
    free (HLIR.MkExprVariable ann _) = Map.singleton ann.name ann.typeValue.runIdentity
    free (HLIR.MkExprLiteral _) = Map.empty
    -- We exclude parameters from the free variables of a lambda
    -- because they are bound variables.
    free (HLIR.MkExprLambda params _ body) =
        free body Map.\\ Map.fromList (map HLIR.unannotate params)
    -- Same rule applies for let-in bindings as name is a bound variable.
    free (HLIR.MkExprLetIn binding value inExpr _) =
        (free value <> free inExpr)
            Map.\\ Map.singleton binding.name binding.typeValue.runIdentity
    free (HLIR.MkExprCondition cond thenB elseB _) = free cond <> free thenB <> free elseB
    free (HLIR.MkExprLocated _ e) = free e
    free (HLIR.MkExprStructureAccess struct _) = free struct
    free (HLIR.MkExprStructureCreation _ fields) =
        free (Map.elems fields)
    free (HLIR.MkExprDereference e _) = free e
    free (HLIR.MkExprReference e _) = free e
    free (HLIR.MkExprUpdate updatable expr _) = free updatable <> free expr
    free (HLIR.MkExprSizeOf _) = Map.empty
    free (HLIR.MkExprSingleIf cond thenB _) = free cond <> free thenB
    free (HLIR.MkExprCast e _) = free e
    free (HLIR.MkExprWhile cond body _ inExpr) = free cond <> free body <> free inExpr
    free (HLIR.MkExprIfIs expr pat thenBr elseBr _) =
        (free expr <> free thenBr <> free elseBr) Map.\\ free pat
    free (HLIR.MkExprFunctionAccess _ this exprs) = free this <> free exprs
    free (HLIR.MkExprWhileIs expr pat body _ inExpr) =
        (free expr <> free body <> free inExpr) Map.\\ free pat
    free (HLIR.MkExprReturn e) = free e
    free HLIR.MkExprBreak = Map.empty
    free HLIR.MkExprContinue = Map.empty

instance Free (HLIR.Pattern Identity HLIR.Type) where
    free (HLIR.MkPatternVariable _) = mempty
    free (HLIR.MkPatternLet ann) = Map.singleton ann.name ann.typeValue.runIdentity
    free (HLIR.MkPatternConstructor _ patterns _) = free patterns
    free (HLIR.MkPatternLiteral _) = Map.empty
    free (HLIR.MkPatternStructure _ fields) = free (Map.elems fields)
    free HLIR.MkPatternWildcard = Map.empty
    free (HLIR.MkPatternLocated _ p) = free p
