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
    let codeLines = map codegenToplevel toplevels
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
codegenToplevel :: MLIR.Toplevel -> Text
codegenToplevel (MLIR.MkTopFunction name params ret body) =
    let paramList = Text.intercalate ", " [codegenType [] ty <> " "<> varify paramName | (MLIR.MkAnnotation paramName ty) <- params]
        retType = codegenType [] ret
        funcHeader = Text.concat [retType, " ", varify name, "(", paramList, ") {"]
        funcBody = map codegenExpression body
        funcFooter = ["}"]
        insertedReturn = case unsnoc funcBody of
            Just (initLines, lastLine) -> map (<> ";") initLines <> [Text.concat ["    return ", lastLine, ";"]]
            Nothing -> []
     in Text.concat ([funcHeader] ++ insertedReturn ++ funcFooter)
codegenToplevel (MLIR.MkTopGlobal name ty (Just expr)) =
    let constType = codegenType [] ty
        constValue = codegenExpression expr
        in Text.concat [constType, " ", varify name, " = ", constValue, ";"]
codegenToplevel (MLIR.MkTopGlobal name ty Nothing) =
    let constType = codegenType [] ty
        in Text.concat [constType, " ", varify name, ";"]
codegenToplevel (MLIR.MkTopPublic n) = do
    let innerCode = codegenToplevel n
     in Text.concat ["// Public\n", innerCode]
codegenToplevel (MLIR.MkTopStructure name fields) =
    let fieldLines = map (\(fName, fType) -> Text.concat ["    ", codegenType [] fType, " ", varify fName, ";"]) fields
        structHeader = Text.concat ["typedef struct ", varify name, " {"]
        structFooter = Text.concat ["} ", varify name, ";"]
     in Text.unlines ([structHeader] ++ fieldLines ++ [structFooter])
codegenToplevel (MLIR.MkTopExternalFunction name generics params ret) = do
    let paramList = Text.intercalate ", " (map (codegenType generics) params)
        retType = codegenType generics ret
     in Text.concat ["extern ", retType, " ", name, "(", paramList, ");"]

-- | Convert a single MLIR expression to C code lines.
-- | This function takes an expression, and returns a list of C code lines.
-- | This is a simplified example and does not cover all MLIR constructs.
-- | You would need to expand this function to handle all the constructs you need.
codegenExpression :: MLIR.Expression -> Text
codegenExpression (MLIR.MkExprLiteral lit) = codegenLiteral lit
codegenExpression (MLIR.MkExprVariable name) = varify name
codegenExpression (MLIR.MkExprApplication f args) =
    let argList = Text.intercalate ", " (map codegenExpression args)
     in Text.concat [codegenExpression f, "(", argList, ")"]
codegenExpression (MLIR.MkExprBlock bl) =
    let blockLines = map codegenExpression bl
     in Text.concat ["{", Text.concat (map (<> ";") blockLines), "}"]
codegenExpression (MLIR.MkExprCast ty expr) =
    Text.concat ["(", codegenType [] ty, ")", codegenExpression expr]
codegenExpression (MLIR.MkExprCondition cond thenBr elseBr) =
    let condExpr = codegenExpression cond
        thenExpr = codegenExpression thenBr
        elseExpr = codegenExpression elseBr
     in Text.concat ["if (", condExpr, ") ", thenExpr, " else ", elseExpr]
codegenExpression (MLIR.MkExprLet name ty (Just expr)) = do
    let varType = codegenType [] ty
        varValue = codegenExpression expr
     in Text.concat [varType, " ", varify name, " = ", varValue, ";"]
codegenExpression (MLIR.MkExprLet name ty Nothing) = do
    let varType = codegenType [] ty
        in Text.concat [varType, " ", name, ";"]
codegenExpression (MLIR.MkExprDereference e) =
    Text.concat ["(*", codegenExpression e, ")"]
codegenExpression (MLIR.MkExprReference e) =
    Text.concat ["(&", codegenExpression e, ")"]
codegenExpression (MLIR.MkExprSizeOf ty) = do
    Text.concat ["sizeof(", codegenType [] ty, ")"]
codegenExpression (MLIR.MkExprStructureAccess (MLIR.MkExprDereference e) f) = do
    Text.concat ["(", codegenExpression e, ")->", varify f]
codegenExpression (MLIR.MkExprStructureAccess e f) = do
    Text.concat [codegenExpression e, ".", varify f]
codegenExpression (MLIR.MkExprStructureCreation ty fields) =
    let fieldInits = map (\(n, v) -> Text.concat [".", n, " = ", codegenExpression v]) (Map.toList fields)
        in Text.concat ["(", codegenType [] ty, ") { ", Text.intercalate ", " fieldInits, " }"]
codegenExpression (MLIR.MkExprUpdate e v) =
    Text.concat [codegenExpression e, " = ", codegenExpression v]
codegenExpression (MLIR.MkExprSingleIf cond thenBranch) = do
    let condExpr = codegenExpression cond
        thenExpr = codegenExpression thenBranch
     in Text.concat ["if (", condExpr, ") ", thenExpr]

-- | Convert a single MLIR literal to a C code string.
-- | This function takes a literal, and returns a C code string.
-- | This is a simplified example and does not cover all MLIR literal types.
-- | You would need to expand this function to handle all the literal types you need.
codegenLiteral :: MLIR.Literal -> Text
codegenLiteral (MLIR.MkLitInt n) = Text.pack (show n)
codegenLiteral (MLIR.MkLitFloat f) = Text.pack (show f)
codegenLiteral (MLIR.MkLitBool True) = "true"
codegenLiteral (MLIR.MkLitBool False) = "false"
codegenLiteral (MLIR.MkLitString s) = Text.concat ["\"", dropDoubleQuotes (show s), "\""]
codegenLiteral (MLIR.MkLitChar c) = Text.concat ["'", dropDoubleQuotes (show (Text.singleton c)), "'"]

dropDoubleQuotes :: Text -> Text
dropDoubleQuotes txt =
    if Text.length txt >= 2 && Text.head txt == '\"' && Text.last txt == '\"'
        then Text.init (Text.tail txt)
        else txt

-- | Convert a single MLIR type to a C type string.
-- | This function takes a type, and returns a C type string.
-- | This is a simplified example and does not cover all MLIR type constructs.
-- | You would need to expand this function to handle all the type constructs you need.
codegenType :: [Text] -> MLIR.Type -> Text
codegenType _ (MLIR.MkTyId "i8") = "int8_t"
codegenType _ (MLIR.MkTyId "i16") = "int16_t"
codegenType _ (MLIR.MkTyId "i32") = "int32_t"
codegenType _ (MLIR.MkTyId "i64") = "int64_t"
codegenType _ (MLIR.MkTyId "u8") = "uint8_t"
codegenType _ (MLIR.MkTyId "u16") = "uint16_t"
codegenType _ (MLIR.MkTyId "u32") = "uint32_t"
codegenType _ (MLIR.MkTyId "u64") = "uint64_t"
codegenType _ (MLIR.MkTyId "f32") = "float"
codegenType _ (MLIR.MkTyId "f64") = "double"
codegenType _ (MLIR.MkTyId "char") = "char"
codegenType _ (MLIR.MkTyId "bool") = "bool"
codegenType generics (MLIR.MkTyId n) | n `elem` generics = "void*"
codegenType _ (MLIR.MkTyId n) = varify n
codegenType _ (MLIR.MkTyVar tv) = do
    let tvr = IO.unsafePerformIO $ readIORef tv

    case tvr of
        MLIR.Link ty -> codegenType [] ty
        MLIR.Unbound name _ -> Err.compilerError $ "Unbound type variable in codegen: " <> name
codegenType generics (MLIR.MkTyPointer ty) = Text.concat [codegenType generics ty, "*"]
codegenType generics (args MLIR.:->: ret) =
    let argTypes = Text.intercalate ", " (map (codegenType generics) args)
        retType = codegenType generics ret
     in Text.concat [retType, " (*)(/* environment */ void*, ", argTypes, ")"]
codegenType generics (MLIR.MkTyAnonymousStructure n _) = codegenType generics n
codegenType _ (MLIR.MkTyQuantified _) = "void*"
codegenType _ (MLIR.MkTyApp _ _) = Err.compilerError "Type applications are not directly supported in C codegen"

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
