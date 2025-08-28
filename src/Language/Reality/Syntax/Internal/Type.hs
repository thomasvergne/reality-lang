{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Reality.Syntax.Internal.Type where

import Data.Map qualified as Map
import Data.Text qualified as T
import GHC.IO qualified as IO
import GHC.Show qualified as S
import Prelude hiding (Type)

-- | LEVEL TYPE
-- | Level represents the level of a type variable. It is used to determine the
-- | scope of a type variable.
type Level = Int

-- | QUANTIFIED VARIABLE TYPE
-- | QuVar represents a generic type defined by the user. For instance "A" in
-- | the following example: "fn id<A>(x: A): A => x".
type QuVar = Text

-- | BONZAI TYPE TYPE
-- | A type is an abstract representation of a value in Bonzai. It is used to check
-- | values correctness at compile time and to infer types when the type is not
-- | explicitly defined.
-- |
-- | A type in Bonzai consists of the following components:
-- | - MkTyId: Represents a type identifier, such as "int", "float", "string", etc.
-- | - MkTyApp: Represents a type application, such as "List<int>", "Tuple<int, float>", etc.
-- | - MkTyVar: Represents a type variable, such as "A", "B", "C", etc.
-- | - MkTyQuantified: Represents a quantified type, such as "forall A. A -> A".
data Type
    = MkTyId Text
    | MkTyApp Type [Type]
    | MkTyVar (IORef TyVar)
    | MkTyQuantified Text
    | MkTyAnonymousStructure (Map Text Type)
    deriving (Ord, Generic)

-- | ORD INSTANCE FOR TYPE
-- | Ord instance is not trivially derivable for the Type type because it contains
-- | a reference to an IORef. So we need to define the Ord instance manually.
-- | To achieve that easily, we can compare the values of the IORefs.
instance (Ord a) => Ord (IORef a) where
    compare a b = compare (IO.unsafePerformIO $ readIORef a) (IO.unsafePerformIO $ readIORef b)

-- | TYPE VARIABLES
-- | Type variable represents a type variable in Bonzai. It can either be a link to
-- | another type or an unbound type variable.
data TyVar
    = Link Type
    | Unbound QuVar Level
    deriving (Eq, Ord, Generic)

-- | TYPE SCHEME
-- | A type scheme is a "type" with bound quantified variables. It is used to
-- | represent polymorphic types in Bonzai. For instance, the following type
-- | scheme represents a polymorphic identity function:
-- |
-- | forall A. A -> A
-- |
-- | It may not contains free variables, in other words, all variables mustn't
-- | escape the scope of the quantifiers.
data Scheme = Forall [QuVar] Type
    deriving (Eq, Show)

-- | EQUALITY INSTANCE FOR TYPE
-- | Equality instance is not trivially derivable for the Type type because it contains
-- | a reference to an IORef. So we need to define the Eq instance manually.
-- | To achieve that easily, we can compare the values of the IORefs.
instance Eq Type where
    MkTyId a == MkTyId b = a == b
    MkTyVar a == MkTyVar b = do
        let a' = IO.unsafePerformIO $ readIORef a
        let b' = IO.unsafePerformIO $ readIORef b
        a' == b'
    MkTyApp a b == MkTyApp c d = a == c && b == d
    _ == _ = False

-- | FUNCTION TYPE
-- | Function type is a type that represents a function in Bonzai. It consists of
-- | a list of argument types and a return type. For instance, the following type
-- | represents a function that takes two integers and returns a float:
-- |
-- | (int, int) -> float
-- |
-- | Represented as:
-- |
-- | MkTyFun [MkTyInt, MkTyInt] MkTyFloat
pattern MkTyFun :: [Type] -> Type -> Type
pattern MkTyFun args retTy = MkTyApp (MkTyId "#func") (retTy : args)

-- | FUNCTION SYNONYM ALIAS
-- | This operator is a function synonym for the MkTyFun constructor. It allows
-- | to pattern match on function types more easily.
pattern (:->:) :: [Type] -> Type -> Type
pattern args :->: retTy = MkTyFun args retTy

-- | PRIMITIVE TYPES
-- | Primitive types are the most basic types in Bonzai. They represent the
-- | basic types such as integers, floats, characters, strings, booleans, and
-- | unit.
pattern MkTyInt, MkTyFloat, MkTyChar, MkTyString, MkTyBool, MkTyUnit :: Type
pattern MkTyInt = MkTyId "int"
pattern MkTyFloat = MkTyId "float"
pattern MkTyChar = MkTyId "char"
pattern MkTyString = MkTyId "string"
pattern MkTyBool = MkTyId "bool"
pattern MkTyUnit = MkTyId "unit"

-- | LIST TYPE
-- | List type is a type that represents a list of values in Bonzai. It is used
-- | to represent a sequence of values of the same type.
pattern MkTyList :: Type -> Type
pattern MkTyList a = MkTyApp (MkTyId "list") [a]

-- | POINTER TYPE
-- | Pointer type is a type that represents a pointer to a value in Bonzai.
-- | It is used to represent a reference to a value of a given type.
pattern MkTyPointer :: Type -> Type
pattern MkTyPointer a = MkTyApp (MkTyId "pointer") [a]

-- | TUPLE TYPE
-- | Tuple type is a type that represents a tuple of values in Bonzai. It is used
-- | to represent a fixed-size collection of values of different types.
pattern MkTyTuple :: Type -> Type -> Type
pattern MkTyTuple a b = MkTyApp (MkTyId "Tuple") [a, b]

instance ToText Type where
    toText (MkTyId a) = a
    toText (args :->: ret) = T.concat ["(", T.intercalate ", " (map toText args), ") -> ", toText ret]
    toText (MkTyTuple a b) = T.concat ["(", toText a, ", ", toText b, ")"]
    toText (MkTyApp a b) = T.concat [toText a, "[", T.intercalate ", " (map toText b), "]"]
    toText (MkTyVar a) = do
        let a' = IO.unsafePerformIO $ readIORef a
        toText a'
    toText (MkTyQuantified a) = a
    toText (MkTyAnonymousStructure a) =
        let fields = Map.toList a
            fieldTexts = map (\(name, ty) -> name <> ": " <> toText ty) fields
         in T.concat ["{ ", T.intercalate ", " fieldTexts, " }"]

-- | TYPE SIMPLIFICATION
-- | Given a type, simplify it by following the links of type variables until
-- | we reach a concrete type.
-- | It is used to remove the Link constructor from a TypeVar and to get the
-- | actual type.
simplify :: (MonadIO m) => Type -> m Type
simplify (MkTyVar a) = do
    a' <- readIORef a
    case a' of
        Link b -> simplify b
        _ -> pure $ MkTyVar a
simplify (MkTyApp a b) = do
    a' <- simplify a
    b' <- mapM simplify b
    pure $ MkTyApp a' b'
simplify a = pure a

instance ToText TyVar where
    toText (Link a) = toText a
    toText (Unbound a l) = a <> "@" <> T.pack (show l)

instance ToText (Maybe Type) where
    toText (Just a) = toText a
    toText Nothing = "infer"

instance ToText Scheme where
    toText (Forall a b) = T.concat ["forall ", T.intercalate ", " a, ". ", toText b]

instance ToText (Identity Type) where
    toText (Identity a) = toText a

instance Show Type where
    show = T.unpack . toText
