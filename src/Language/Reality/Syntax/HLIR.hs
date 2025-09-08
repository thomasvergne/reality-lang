{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Reality.Syntax.HLIR (
    Expression (..),
    Toplevel (..),
    Pattern (..),
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
    -- Functions
    getFirstAnnotationArgument,
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
        , returnType :: f t
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
        , returnType :: f t
        }
    | MkExprCondition
        { condition :: Expression f t
        , thenBranch :: Expression f t
        , elseBranch :: Expression f t
        , returnType :: f t
        }
    | MkExprSingleIf (Expression f t) (Expression f t) (f t)
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
    | MkExprDereference (Expression f t) (f t)
    | MkExprReference (Expression f t) (f t)
    | MkExprUpdate
        { update :: Expression f t
        , value :: Expression f t
        , updateType :: f t
        }
    | MkExprSizeOf t
    | MkExprCast (Expression f t) t
    | MkExprWhile
        { condition :: Expression f t
        , body :: Expression f t
        , returnType :: f t
        , inExpr :: Expression f t
        }
    | MkExprIfIs
        { expr :: Expression f t
        , expectedPattern :: Pattern f t
        , thenBranch :: Expression f t
        , maybeElseBranch :: Maybe (Expression f t)
        , returnType :: f t
        }
    | MkExprWhileIs
        { expr :: Expression f t
        , expectedPattern :: Pattern f t
        , body :: Expression f t
        , returnType :: f t
        , inExpr :: Expression f t
        }
    | MkExprFunctionAccess
        { field :: Text
        , fieldExpr :: Expression f t
        , arguments :: [Expression f t]
        }
    | MkExprReturn (Expression f t)
    | MkExprBreak
    | MkExprContinue
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
    | MkTopAnnotation [Expression f t] (Toplevel f t)
    | MkTopExternLet (Ann.Annotation t)
    | MkTopEnumeration
        { name :: Ann.Annotation [Text]
        , constructors :: Map Text (Maybe [t])
        }
    deriving (Eq, Ord, Generic)

-- | PATTERN
data Pattern f t
    = MkPatternVariable (Ann.Annotation (f t))
    | MkPatternLet (Ann.Annotation (f t))
    | MkPatternLiteral Lit.Literal
    | MkPatternWildcard
    | MkPatternStructure t (Map Text (Pattern f t))
    | MkPatternConstructor Text [Pattern f t] (f t) -- Constructor name and its associated patterns
    | MkPatternLocated
        { span :: Position
        , patternNode :: Pattern f t
        }
    deriving (Eq, Ord, Generic)

-- | BINARY EXPRESSION PATTERN
-- | A pattern synonym to represent binary expressions in Bonzai.
pattern MkExprBinary ::
    Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprBinary op a b =
    MkExprApplication (MkExprVariable (MkAnnotation op Nothing) []) [a, b] Nothing

pattern MkExprVarCall ::
    Text -> [Expression Maybe t] -> Expression Maybe t
pattern MkExprVarCall name args =
    MkExprApplication (MkExprVariable (MkAnnotation name Nothing) []) args Nothing

-- |  STRING EXPRESSION PATTERN
-- | A pattern synonym to represent string expressions in Bonzai.
pattern MkExprString :: Text -> Expression f t
pattern MkExprString s = MkExprLiteral (MkLitString s)

-- | TUPLE EXPRESSION PATTERN
-- | A pattern synonym to represent tuple expressions in Bonzai.
pattern MkExprTuple ::
    Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprTuple a b =
    MkExprApplication
        (MkExprVariable (MkAnnotation "Tuple" Nothing) [])
        [a, b]
        Nothing

type family HLIR (s :: Symbol) where
    HLIR "expression" = Expression Maybe Type
    HLIR "toplevel" = Toplevel Maybe Type
    HLIR "pattern" = Pattern Maybe Type

type family TLIR (s :: Symbol) where
    TLIR "expression" = Expression Identity Type
    TLIR "toplevel" = Toplevel Identity Type
    TLIR "pattern" = Pattern Identity Type

getFirstAnnotationArgument :: Expression f t -> Maybe Text
getFirstAnnotationArgument (MkExprVariable ann _) = pure ann.name
getFirstAnnotationArgument (MkExprLocated _ e) = getFirstAnnotationArgument e
getFirstAnnotationArgument _ = Nothing

instance Locate (Expression f t) where
    locate e p = MkExprLocated{span = p, expr = e}

instance Locate (Toplevel f t) where
    locate e p = MkTopLocated{span = p, node = e}

instance Locate (Pattern f t) where
    locate p pos = MkPatternLocated{span = pos, patternNode = p}

instance (ToText (f t), ToText t) => ToText (Expression f t) where
    toText (MkExprApplication callee args _) =
        T.concat [toText callee, "(", T.intercalate ", " (map toText args), ")"]
    toText (MkExprVariable ann _) = toText ann.name
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
    toText (MkExprLetIn binding value inExpr _) =
        T.concat
            [ "let "
            , toText binding
            , " = "
            , toText value
            , " in "
            , toText inExpr
            ]
    toText (MkExprCondition cond thenB elseB _) =
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
    toText (MkExprDereference e _) = T.concat ["*", toText e]
    toText (MkExprReference e _) = T.concat ["&", toText e]
    toText (MkExprUpdate update value _) =
        T.concat [toText update, " = ", toText value]
    toText (MkExprSizeOf t) = T.concat ["sizeof(", toText t, ")"]
    toText (MkExprSingleIf cond thenB _) =
        T.concat ["if ", toText cond, " then ", toText thenB]
    toText (MkExprCast e t) = T.concat ["(", toText e, " as ", toText t, ")"]
    toText (MkExprWhile cond body _ inExpr) =
        T.concat
            [ "while "
            , toText cond
            , " { "
            , toText body
            , " } in "
            , toText inExpr
            ]
    toText (MkExprIfIs expr pat thenB maybeElseB _) =
        let elseText = case maybeElseB of
                Just elseB -> " else " <> toText elseB
                Nothing -> ""
         in T.concat
                ["if ", toText expr, " is ", toText pat, " then ", toText thenB, elseText]
    toText (MkExprFunctionAccess func fieldExpr args) =
        T.concat
            [ toText func
            , "("
            , toText fieldExpr
            , ")("
            , T.intercalate ", " (map toText args)
            , ")"
            ]
    toText (MkExprWhileIs expr pat body _ inExpr) =
        T.concat
            [ "while "
            , toText expr
            , " is "
            , toText pat
            , " { "
            , toText body
            , " } in "
            , toText inExpr
            ]
    toText (MkExprReturn expr) = "return " <> toText expr
    toText MkExprBreak = "break"
    toText MkExprContinue = "continue"

instance (ToText (f t), ToText t) => ToText (Pattern f t) where
    toText (MkPatternVariable ann) = toText ann
    toText (MkPatternLet ann) = "let " <> toText ann
    toText (MkPatternLiteral lit) = toText lit
    toText MkPatternWildcard = "_"
    toText (MkPatternStructure t fields) =
        let fieldTexts = map (\(name, pat) -> name <> ": " <> toText pat) (Map.toList fields)
         in T.concat ["{ ", T.intercalate ", " fieldTexts, " } :: ", toText t]
    toText (MkPatternConstructor name pats _) =
        T.concat [name, "(", T.intercalate ", " (map toText pats), ")"]
    toText (MkPatternLocated _ p) = toText p

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
    toText (MkTopAnnotation exprs node) =
        T.concat
            [ "#["
            , T.intercalate ", " (map toText exprs)
            , "]\n"
            , toText node
            ]
    toText (MkTopExternLet binding) =
        T.concat ["extern let ", toText binding]
    toText (MkTopEnumeration name constructors) =
        let constructorTexts = map formatConstructor (Map.toList constructors)
            formatConstructor (cName, Nothing) = cName
            formatConstructor (cName, Just ty) = cName <> "(" <> T.intercalate ", " (map toText ty) <> ")"
         in T.concat
                [ "enum "
                , name.name
                , "["
                , T.intercalate ", " name.typeValue
                , "]"
                , " { "
                , T.intercalate ", " constructorTexts
                , " }"
                ]
