{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Reality.Syntax.HLIR (
    Expression (..),
    Toplevel (..),
    -- Patterns
    pattern MkExprBinary,
    pattern MkExprString,
    pattern MkExprTuple,
    pattern MkExprVarCall,
    -- Re-exports
    module Lit,
    module Ann,
    module Pos,
    module Ty,
    -- Type families
    HLIR,
    TLIR,
)
where

import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.TypeLits (Symbol)
import Language.Reality.Syntax.Internal.Annotation as Ann
import Language.Reality.Syntax.Internal.Literal as Lit
import Language.Reality.Syntax.Internal.Position as Pos
import Language.Reality.Syntax.Internal.Type as Ty
import Prelude hiding (Type)

data Expression f t
    = MkExprApplication
        { callee :: Expression f t
        , arguments :: [Expression f t]
        }
    | MkExprVariable (Ann.Annotation (f t)) [t]
    | MkExprLiteral Lit.Literal
    | MkExprLambda
        { parameters :: [Ann.Annotation (f t)]
        , returnType :: f t
        , body :: Expression f t
        }
    | MkExprLetIn
        { binding :: Ann.Annotation (f t)
        , value :: Expression f t
        , inExpr :: Expression f t
        }
    | MkExprCondition
        { condition :: Expression f t
        , thenBranch :: Expression f t
        , elseBranch :: Expression f t
        }
    | MkExprLocated
        { span :: Position
        , expr :: Expression f t
        }
    | MkExprStructureAccess
        { structure :: Expression f t
        , field :: Text
        }
    | MkExprStructureCreation
        { annotation :: t
        , fields :: Map Text (Expression f t)
        }
    deriving (Eq, Ord, Generic)

data Toplevel f t
    = MkTopConstantDeclaration
        { binding :: Ann.Annotation t
        , value :: Expression f t
        }
    | MkTopFunctionDeclaration
        { name :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        , body :: Expression f t
        }
    | MkTopImport [Text]
    | MkTopTypeAlias
        { name :: Ann.Annotation [Text]
        , boundType :: t
        }
    | MkTopPublic (Toplevel f t)
    | MkTopModuleDeclaration
        { moduleName :: Text
        , nodes :: [Toplevel f t]
        }
    | MkTopLocated
        { span :: Position
        , node :: Toplevel f t
        }
    | MkTopStructureDeclaration
        { header :: Ann.Annotation [Text]
        , fields :: Map Text t
        }
    | MkTopExternalFunction
        { name :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        }
    | MkTopProperty
        { header :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        }
    | MkTopImplementation
        { forType :: Ann.Annotation t
        , header :: Ann.Annotation [Text]
        , parameters :: [Ann.Annotation t]
        , returnType :: t
        , body :: Expression f t
        }
    deriving (Eq, Ord, Generic)

-- | BINARY EXPRESSION PATTERN
-- | A pattern synonym to represent binary expressions in Bonzai.
pattern MkExprBinary ::
    Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprBinary op a b =
    MkExprApplication (MkExprVariable (MkAnnotation op Nothing) []) [a, b]

pattern MkExprVarCall ::
    Text -> [Expression Maybe t] -> Expression Maybe t
pattern MkExprVarCall name args =
    MkExprApplication (MkExprVariable (MkAnnotation name Nothing) []) args

-- |  STRING EXPRESSION PATTERN
-- | A pattern synonym to represent string expressions in Bonzai.
pattern MkExprString :: Text -> Expression f t
pattern MkExprString s = MkExprLiteral (MkLitString s)

-- | TUPLE EXPRESSION PATTERN
-- | A pattern synonym to represent tuple expressions in Bonzai.
pattern MkExprTuple ::
    Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprTuple a b =
    MkExprApplication (MkExprVariable (MkAnnotation "Tuple" Nothing) []) [a, b]

type family HLIR (s :: Symbol) where
    HLIR "expression" = Expression Maybe Type
    HLIR "toplevel" = Toplevel Maybe Type

type family TLIR (s :: Symbol) where
    TLIR "expression" = Expression Identity Type
    TLIR "toplevel" = Toplevel Identity Type

instance Locate (Expression f t) where
    locate e p = MkExprLocated{span = p, expr = e}

instance Locate (Toplevel f t) where
    locate e p = MkTopLocated{span = p, node = e}

instance (ToText (f t), ToText t) => ToText (Expression f t) where
    toText (MkExprApplication callee args) =
        T.concat [toText callee, "(", T.intercalate ", " (map toText args), ")"]
    toText (MkExprVariable ann vars) = toText ann <> "::[" <> T.intercalate ", " (map toText vars) <> "]"
    toText (MkExprLiteral lit) = toText lit
    toText (MkExprLambda params ret body) =
        T.concat
            [ "\\("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            , " { "
            , toText body
            , " }"
            ]
    toText (MkExprLetIn binding value inExpr) =
        T.concat
            [ "let "
            , toText binding
            , " = "
            , toText value
            , " in "
            , toText inExpr
            ]
    toText (MkExprCondition cond thenB elseB) =
        T.concat
            [ "if "
            , toText cond
            , " then "
            , toText thenB
            , " else "
            , toText elseB
            ]
    toText (MkExprLocated _ e) = toText e
    toText (MkExprStructureAccess struct field) =
        T.concat [toText struct, ".", field]
    toText (MkExprStructureCreation ann fields) =
        let fieldTexts = map (\(name, expr) -> name <> ": " <> toText expr) (Map.toList fields)
         in T.concat ["{ ", T.intercalate ", " fieldTexts, " } :: ", toText ann]

instance (ToText (f t), ToText t) => ToText (Toplevel f t) where
    toText (MkTopConstantDeclaration binding value) =
        T.concat ["const ", toText binding, " = ", toText value]
    toText (MkTopFunctionDeclaration name params ret body) =
        T.concat
            [ "fn "
            , name.name
            , "["
            , T.intercalate ", " name.typeValue
            , "]"
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            , " { "
            , toText body
            , " }"
            ]
    toText (MkTopImport mods) =
        T.concat ["import ", T.intercalate "::" mods]
    toText (MkTopTypeAlias name typeValue) =
        T.concat
            [ "type "
            , name.name
            , "["
            , T.intercalate ", " name.typeValue
            , "]"
            , " = "
            , toText typeValue
            ]
    toText (MkTopPublic node) = T.concat ["public ", toText node]
    toText (MkTopModuleDeclaration modName nodes) =
        T.concat
            [ "mod "
            , modName
            , " { "
            , T.intercalate "\n" (map toText nodes)
            , " }"
            ]
    toText (MkTopLocated _ n) = toText n
    toText (MkTopStructureDeclaration header fields) =
        let fieldTexts = map (\(name, ty) -> name <> ": " <> toText ty) (Map.toList fields)
         in T.concat
                [ "struct "
                , header.name
                , "["
                , T.intercalate ", " header.typeValue
                , "]"
                , " { "
                , T.intercalate ", " fieldTexts
                , " }"
                ]
    toText (MkTopExternalFunction name params ret) =
        T.concat
            [ "extern fn "
            , name.name
            , "["
            , T.intercalate ", " name.typeValue
            , "]"
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            ]
    toText (MkTopProperty header params ret) =
        T.concat
            [ "property "
            , header.name
            , "["
            , T.intercalate ", " header.typeValue
            , "]"
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText ret
            ]
    toText (MkTopImplementation forType header params returnType body) =
        T.concat
            [ "impl fn "
            , header.name
            , "["
            , T.intercalate ", " header.typeValue
            , "] for "
            , toText forType
            , "("
            , T.intercalate ", " (map toText params)
            , ") -> "
            , toText returnType
            , " { "
            , toText body
            , " }"
            ]
