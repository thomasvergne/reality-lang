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
    free (HLIR.MkExprApplication f args) = free f <> free args
    free (HLIR.MkExprVariable ann _) = Map.singleton ann.name ann.typeValue.runIdentity
    free (HLIR.MkExprLiteral _) = Map.empty
    free (HLIR.MkExprLambda params _ body) =
        free body Map.\\ Map.fromList (map HLIR.unannotate params)
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
