module Language.Reality.Frontend.Parser (
    module P,
    module M,
    module MCL,
    ParseError,
    parseRealityFile,
    parseRealityTestFile,
) where

import Control.Monad.Parser as P
import Text.Megaparsec as M hiding (ParseError)
import Text.Megaparsec.Char.Lexer as MCL

type ParseError = ParseErrorBundle Text Void

parseRealityFile ::
    (MonadIO m) =>
    FilePath ->
    FileContent ->
    P.Parser m a ->
    m (Either ParseError a)
parseRealityFile filePath fileContent p =
    P.parseContent p filePath fileContent

parseRealityTestFile ::
    (MonadIO m) =>
    FileContent ->
    P.Parser m a ->
    m (Either ParseError a)
parseRealityTestFile = flip P.parseTestContent
