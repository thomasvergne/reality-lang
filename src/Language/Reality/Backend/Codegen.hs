module Language.Reality.Backend.Codegen where

import Language.Reality.Syntax.MLIR qualified as MLIR
import Data.Text qualified as Text
import Data.Map qualified as Map
import Data.Char qualified as Char
import Control.Monad.Result qualified as Err
import Text.Printf qualified as Text
import GHC.IO qualified as IO

-- | CODEGEN
-- | Convert MLIR to a C code string.
codegenProgram :: MonadIO m => [MLIR.Toplevel] -> m Text
codegenProgram toplevels = do
    codeLines <- mapM codegenToplevel toplevels
    pure (Text.unlines codeLines)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x:xs) = case unsnoc xs of
    Just (init', last') -> Just (x:init', last')
    Nothing -> Nothing

-- | Convert a single MLIR toplevel node to C code lines.
-- | This function takes a toplevel node, and returns a list of C code lines.
-- | This is a simplified example and does not cover all MLIR constructs.
-- | You would need to expand this function to handle all the constructs you need.
codegenToplevel :: MonadIO m => MLIR.Toplevel -> m Text
codegenToplevel (MLIR.MkTopFunction name params ret body) = do
    paramList <- fmap (Text.intercalate ", ") $ mapM (\(MLIR.MkAnnotation paramName ty) -> codegenType True (Just paramName) [] ty) params
    retType <- codegenType True Nothing [] ret
    let funcHeader = Text.concat [retType, " ", varify name, "(", paramList, ") {"]
    funcBody <- mapM codegenExpression body
    let funcFooter = ["}"]
    let insertedReturn = case unsnoc funcBody of
            Just (initLines, lastLine) -> map (<> ";") initLines <> [Text.concat ["    return ", lastLine, ";"]]
            Nothing -> []
    pure $ Text.concat ([funcHeader] ++ insertedReturn ++ funcFooter)
codegenToplevel (MLIR.MkTopGlobal name ty (Just expr)) = do
    constType <- codegenType True (Just name) [] ty
    constValue <- codegenExpression expr
    pure $ Text.concat [constType, " = ", constValue, ";"]
codegenToplevel (MLIR.MkTopGlobal name ty Nothing) = do
    constType <- codegenType True (Just name) [] ty
    pure $ Text.concat [constType, ";"]
codegenToplevel (MLIR.MkTopPublic n) = do
    innerCode <- codegenToplevel n
    pure $ Text.concat ["// Public\n", innerCode]
codegenToplevel (MLIR.MkTopStructure name fields) = do
    fieldLines <- mapM (\(fName, fType) -> do
        fTypeStr <- codegenType False (Just fName) [] fType
        pure $ Text.concat ["    ", fTypeStr, ";"]) fields
    let structHeader = Text.concat ["typedef struct ", varify name, " {"]
    let structFooter = Text.concat ["} ", varify name, ";"]
    pure $ Text.unlines ([structHeader] ++ fieldLines ++ [structFooter])
codegenToplevel (MLIR.MkTopExternalFunction name generics params ret) = do
    paramList <- fmap (Text.intercalate ", ") $ mapM (\p -> codegenType True Nothing generics p) params
    retType <- codegenType True (Just name) generics ret
    pure $ Text.concat ["extern ", retType, "(", paramList, ");"]

-- | Convert a single MLIR expression to C code lines.
-- | This function takes an expression, and returns a list of C code lines.
-- | This is a simplified example and does not cover all MLIR constructs.
-- | You would need to expand this function to handle all the constructs you need.
codegenExpression :: MonadIO m => MLIR.Expression -> m Text
codegenExpression (MLIR.MkExprLiteral lit) = codegenLiteral lit
codegenExpression (MLIR.MkExprVariable name) = pure $ varify name
codegenExpression (MLIR.MkExprApplication f args) = do
    fStr <- codegenExpression f
    argList <- fmap (Text.intercalate ", ") $ mapM codegenExpression args
    pure $ Text.concat [fStr, "(", argList, ")"]
codegenExpression (MLIR.MkExprBlock bl) = do
    blockLines <- mapM codegenExpression bl
    pure $ Text.concat ["{", Text.concat (map (<> ";") blockLines), "}"]
codegenExpression (MLIR.MkExprCast ty expr) = do
    tyStr <- codegenType True Nothing [] ty
    exprStr <- codegenExpression expr
    pure $ Text.concat ["(", tyStr, ")", exprStr]
codegenExpression (MLIR.MkExprCondition cond thenBr elseBr) = do
    condExpr <- codegenExpression cond
    thenExpr <- codegenExpression thenBr
    elseExpr <- codegenExpression elseBr
    pure $ Text.concat ["if (", condExpr, ") ", thenExpr, " else ", elseExpr]
codegenExpression (MLIR.MkExprLet name ty (Just expr)) = do
    varType <- codegenType True (Just name) [] ty
    varValue <- codegenExpression expr
    pure $ Text.concat [varType, " = ", varValue, ";"]
codegenExpression (MLIR.MkExprLet name ty Nothing) = do
    varType <- codegenType True (Just name) [] ty
    pure $ Text.concat [varType, ";"]
codegenExpression (MLIR.MkExprDereference e) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["(*", eStr, ")"]
codegenExpression (MLIR.MkExprReference e) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["(&", eStr, ")"]
codegenExpression (MLIR.MkExprSizeOf ty) = do
    tyStr <- codegenType True Nothing [] ty
    pure $ Text.concat ["sizeof(", tyStr, ")"]
codegenExpression (MLIR.MkExprStructureAccess (MLIR.MkExprDereference e) f) = do
    eStr <- codegenExpression e
    pure $ Text.concat ["(", eStr, ")->", varify f]
codegenExpression (MLIR.MkExprStructureAccess e f) = do
    eStr <- codegenExpression e
    pure $ Text.concat [eStr, ".", varify f]
codegenExpression (MLIR.MkExprStructureCreation ty fields) = do
    tyStr <- codegenType True Nothing [] ty
    fieldInits <- mapM (\(n, v) -> do
        vStr <- codegenExpression v
        pure $ Text.concat [".", n, " = ", vStr]) (Map.toList fields)
    pure $ Text.concat ["(", tyStr, ") { ", Text.intercalate ", " fieldInits, " }"]
codegenExpression (MLIR.MkExprUpdate e v) = do
    eStr <- codegenExpression e
    vStr <- codegenExpression v
    pure $ Text.concat [eStr, " = ", vStr]
codegenExpression (MLIR.MkExprSingleIf cond thenBranch) = do
    condExpr <- codegenExpression cond
    thenExpr <- codegenExpression thenBranch
    pure $ Text.concat ["if (", condExpr, ") ", thenExpr]
codegenExpression (MLIR.MkExprWhile cond body) = do
    condExpr <- codegenExpression cond
    bodyExpr <- codegenExpression body
    pure $ Text.concat ["while (", condExpr, ") { ", bodyExpr, " } "]

-- | Convert a single MLIR literal to a C code string.
-- | This function takes a literal, and returns a C code string.
-- | This is a simplified example and does not cover all MLIR literal types.
-- | You would need to expand this function to handle all the literal types you need.
codegenLiteral :: MonadIO m => MLIR.Literal -> m Text
codegenLiteral (MLIR.MkLitInt n) = pure $ Text.pack (show n)
codegenLiteral (MLIR.MkLitFloat f) = pure $ Text.pack (show f)
codegenLiteral (MLIR.MkLitBool True) = pure "true"
codegenLiteral (MLIR.MkLitBool False) = pure "false"
codegenLiteral (MLIR.MkLitString s) = do
    let sShow = show s
    inner <- dropDoubleQuotes (Text.pack sShow)
    pure $ Text.concat ["\"", inner, "\""]
codegenLiteral (MLIR.MkLitChar c) = do
    let cShow = show (Text.singleton c)
    inner <- dropDoubleQuotes (Text.pack cShow)
    pure $ Text.concat ["'", inner, "'"]

dropDoubleQuotes :: MonadIO m => Text -> m Text
dropDoubleQuotes txt =
    if Text.length txt >= 2 && Text.head txt == '\"' && Text.last txt == '\"'
        then pure $ Text.init (Text.tail txt)
        else pure txt

-- | Convert a single MLIR type to a C type string.
-- | This function takes a type, and returns a C type string.
-- | This is a simplified example and does not cover all MLIR type constructs.
-- | You would need to expand this function to handle all the type constructs you need.
codegenType :: MonadIO m => Bool -> Maybe Text -> [Text] -> MLIR.Type -> m Text
codegenType _ def _ (MLIR.MkTyId "i8") = pure $ Text.concat ["int8_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "i16") = pure $ Text.concat ["int16_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "i32") = pure $ Text.concat ["int32_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "i64") = pure $ Text.concat ["int64_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "u8") = pure $ Text.concat ["uint8_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "u16") = pure $ Text.concat ["uint16_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "u32") = pure $ Text.concat ["uint32_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "u64") = pure $ Text.concat ["uint64_t ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "f32") = pure $ Text.concat ["float ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "f64") = pure $ Text.concat ["double ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "char") = pure $ Text.concat ["char ", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId "bool") = pure $ Text.concat ["bool ", fromMaybe "" def]
codegenType _ def generics (MLIR.MkTyId n) | n `elem` generics = pure $ Text.concat ["void*", fromMaybe "" def]
codegenType _ def _ (MLIR.MkTyId n) = pure $ varify n <> maybe "" (" " <>) def
codegenType shouldPutEnv def _ (MLIR.MkTyVar tv) = do
    let tvr = IO.unsafePerformIO $ readIORef tv

    case tvr of
        MLIR.Link ty -> codegenType shouldPutEnv def [] ty
        MLIR.Unbound name _ -> Err.compilerError $ "Unbound type variable in codegen: " <> name
codegenType shouldPutEnv def generics (MLIR.MkTyPointer ty) = do
    innerTy <- codegenType shouldPutEnv Nothing generics ty
    pure $ Text.concat [innerTy, "*", fromMaybe "" def]
codegenType shouldPutEnv def generics (args MLIR.:->: ret) = do
    argTypes <- Text.intercalate ", " <$> mapM (codegenType shouldPutEnv Nothing generics) args
    retType <- codegenType shouldPutEnv Nothing generics ret

    if shouldPutEnv
        then pure $ Text.concat ["void*", fromMaybe "" def]
        else pure $ Text.concat [retType, " (*", fromMaybe "" def, ")(", argTypes, ")"]
codegenType shouldPutEnv def generics (MLIR.MkTyAnonymousStructure n _) = do
    n' <- codegenType shouldPutEnv Nothing generics n

    pure $ Text.concat [n', " ", fromMaybe "" def]
codegenType _ _ _ (MLIR.MkTyQuantified _) = pure "void*"
codegenType _ _ _ (MLIR.MkTyApp _ _) = Err.compilerError "Type applications are not directly supported in C codegen"

isIdent :: Char -> Bool
isIdent x = Char.isAlphaNum x || x == '_' || x == '$'

varify :: Text -> Text
varify = Text.concatMap (\x' -> if isIdent x' then toText [x'] else fromString (show (ord x')))

encodeUnicode16 :: Text -> Text
encodeUnicode16 = Text.concatMap escapeChar
  where
    escapeChar c
      | c == '\"' = "\\\""
      | c == '\'' = "\\\'"
      | c == '\\' = "\\\\"
      | ' ' <= c && c <= 'z' = toText [c]
      | Char.isPrint c = toText [c]
      | otherwise = Text.pack (Text.printf "\\u%04x" (ord c))
