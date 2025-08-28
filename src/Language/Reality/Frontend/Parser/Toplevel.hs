module Language.Reality.Frontend.Parser.Toplevel where

import Data.List qualified as List
import Data.Map qualified as Map
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Expression qualified as P
import Language.Reality.Frontend.Parser.Internal.Type qualified as Typ
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR
import Text.Megaparsec.Char qualified as P

parseTopConstantDeclaration ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopConstantDeclaration = do
    ((start, _), _) <- Lex.reserved "const"
    idt <- P.parseAnnotation' (snd <$> Typ.parseType)
    ((_, end), expr) <- Lex.symbol "=" *> P.parseExprFull

    pure ((start, end), HLIR.MkTopConstantDeclaration idt expr)

parseTopFunctionDeclaration ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopFunctionDeclaration = do
    ((start, _), _) <- Lex.reserved "fn"
    (_, idt) <- Lex.identifier

    generics <-
        P.option mempty
            $ snd <$> Lex.brackets (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    (_, params) <-
        Lex.parens (P.sepBy (P.parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

    (_, ret) <- Lex.symbol "->" *> Typ.parseType

    ((_, end), body) <- P.parseExprBlock

    pure
        ( (start, end)
        , HLIR.MkTopFunctionDeclaration
            { HLIR.name = HLIR.MkAnnotation idt generics
            , HLIR.parameters = params
            , HLIR.returnType = ret
            , HLIR.body = body
            }
        )

parseTopTypeAlias ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopTypeAlias = do
    ((start, _), _) <- Lex.reserved "type"

    (_, idt) <- Lex.identifier

    generics <-
        P.option mempty
            $ snd <$> Lex.brackets (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    ((_, end), aliased) <- Lex.symbol "=" *> Typ.parseType

    pure
        ( (start, end)
        , HLIR.MkTopTypeAlias
            { HLIR.name = HLIR.MkAnnotation idt generics
            , HLIR.typeValue = aliased
            }
        )

parseTopImport ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopImport = do
    ((start, _), _) <- Lex.reserved "import"
    modules <-
        P.sepBy1 (Lex.symbol "*" <|> Lex.nonLexedID) (P.string "::") P.<?> "module path"

    let lastPosition = if null modules then start else List.maximum (map (snd . fst) modules)
    let moduleParts = map snd modules

    pure ((start, lastPosition), HLIR.MkTopImport moduleParts)

parseTopPublic ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopPublic = do
    ((start, _), _) <- Lex.reserved "pub"
    (pos, node) <- parseTopFull
    pure ((start, snd pos), HLIR.MkTopPublic node)

parseTopModuleDeclaration ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopModuleDeclaration = do
    ((start, _), _) <- Lex.reserved "mod"
    (_, idt) <- Lex.identifier
    ((_, end), nodes) <-
        Lex.braces (P.many (snd <$> parseTopFull <* P.optional Lex.semi))
    pure ((start, end), HLIR.MkTopModuleDeclaration idt nodes)

parseTopStructureDeclaration ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopStructureDeclaration = do
    ((start, _), _) <- Lex.reserved "struct"

    (_, idt) <- Lex.identifier
    generics <- P.option mempty $ do
        (_, gens) <- Lex.brackets (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)
        pure gens

    ((_, end), fields) <-
        Lex.braces $ Map.fromList <$> P.sepBy1 parseField Lex.comma
    pure
        ( (start, end)
        , HLIR.MkTopStructureDeclaration
            { HLIR.header = HLIR.MkAnnotation idt generics
            , HLIR.fields = fields
            }
        )
  where
    parseField = do
        (_, name) <- Lex.identifier
        void Lex.colon
        ty <- snd <$> Typ.parseType
        pure (name, ty)

parseTopExternalFunction ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopExternalFunction = do
    ((start, _), _) <- Lex.reserved "extern"
    ((_, end), _) <- Lex.reserved "fn"
    (_, idt) <- Lex.identifier

    generics <-
        P.option mempty
            $ snd <$> Lex.brackets (P.sepBy1 (snd <$> Lex.identifier) Lex.comma)

    (_, params) <-
        Lex.parens (P.sepBy (P.parseAnnotation' (snd <$> Typ.parseType)) Lex.comma)

    (_, ret) <- Lex.symbol "->" *> Typ.parseType

    pure
        ( (start, end)
        , HLIR.MkTopExternalFunction
            { HLIR.name = HLIR.MkAnnotation idt generics
            , HLIR.parameters = params
            , HLIR.returnType = ret
            }
        )

-- | TOP LEVEL PARSING
-- | A top-level parser is a parser that parses top-level constructs in a
-- | programming language. Top-level constructs are constructs that are not
-- | nested inside other constructs. For instance, in Bonzai, top-level
-- | constructs are:
-- | - Constant declarations
-- | - Function declarations
-- | - Type aliases
-- | - Import statements
-- | - Public declarations
-- | - Module declarations
-- | - Structure declarations
-- | - External function declarations
-- |
-- | The top-level parser is responsible for parsing these constructs and
-- | returning them as a list of top-level nodes.
parseTopFull :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "toplevel")
parseTopFull =
    Lex.locateWith
        <$> P.choice
            [ parseTopConstantDeclaration
            , parseTopFunctionDeclaration
            , parseTopTypeAlias
            , parseTopImport
            , parseTopPublic
            , parseTopModuleDeclaration
            , parseTopStructureDeclaration
            , parseTopExternalFunction
            ]

-- | Parse a complete Bonzai source file.
-- | A Bonzai source file is a sequence of top-level constructs.
-- | The parser will return a list of top-level nodes.
parseProgram :: (MonadIO m) => P.Parser m [HLIR.HLIR "toplevel"]
parseProgram = P.many (snd <$> parseTopFull <* P.optional Lex.semi) <* P.eof
