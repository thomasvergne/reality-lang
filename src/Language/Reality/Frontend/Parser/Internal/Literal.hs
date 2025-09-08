module Language.Reality.Frontend.Parser.Internal.Literal where

import Data.Text qualified as T
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec.Char qualified as MC

-- | DECIMAL
-- | Parse a decimal number with optional underscores as separators.
decimal :: (MonadIO m) => P.Parser m Integer
decimal = do
    parts :: String <- concat <$> P.sepBy1 (some digitChar) (MC.char '_')

    case readEither parts of
        Left _ -> fail "Invalid integer"
        Right x -> pure x

-- | INTEGER
-- | Parse an integer literal.
parseInteger :: (MonadIO m) => P.Parser m Integer
parseInteger =
    P.choice
        [ MC.char '0'
            >> P.choice
                [ MC.char' 'x' >> P.hexadecimal
                , MC.char' 'b' >> P.binary
                , MC.char' 'o' >> P.octal
                , P.signed (pure ()) decimal
                , pure 0
                ]
        , P.signed (pure ()) decimal
        ]

-- | FLOAT
-- | Parse a floating-point number with optional underscores as separators.
parseFloat :: (MonadIO m) => P.Parser m Double
parseFloat = P.signed (pure ()) $ do
    int <- concat <$> P.sepBy1 (some digitChar) (MC.char '_')
    void $ MC.char '.'
    frac <- concat <$> P.sepBy1 (some digitChar) (MC.char '_')

    case readEither (int <> "." <> frac) of
        Left _ -> fail "Invalid float"
        Right y -> pure y

-- | CHARACTER
-- | Parse a character literal.
parseChar :: (MonadIO m) => P.Parser m Char
parseChar = MC.char '\'' *> P.charLiteral <* MC.char '\''

-- | STRING
-- | Parse a string literal.
parseString :: (MonadIO m) => P.Parser m Text
parseString = MC.char '"' *> P.manyTill P.charLiteral (MC.char '"') <&> T.pack

-- | LITERAL
-- | Parse a literal.
-- | Literals are the most basic form of data in a programming language. They
-- | are also called atomic or primitive values. Literals are used to represent
-- | numbers, strings, characters, booleans, and other basic types.
-- |
-- | - Int: An integer literal of the form 0, 1, 2, 3, etc.
-- | - Float: A floating-point literal of the form 0.0, 1.0, 2.0, 3.0, etc.
-- | - Char: A character literal of the form 'a', 'b', 'c', etc.
-- | - String: A string literal of the form "hello", "world", "bonzai", etc.
-- | - Bool: A boolean literal of the form true or false.
parseLiteral :: (MonadIO m) => P.Parser m HLIR.Literal
parseLiteral =
    P.choice
        [ P.try $ HLIR.MkLitFloat <$> parseFloat
        , P.try $ HLIR.MkLitInt <$> parseInteger
        , P.try $ HLIR.MkLitBool True <$ Lex.reserved "true"
        , P.try $ HLIR.MkLitBool False <$ Lex.reserved "false"
        , HLIR.MkLitChar <$> parseChar
        , HLIR.MkLitString <$> parseString
        ]
