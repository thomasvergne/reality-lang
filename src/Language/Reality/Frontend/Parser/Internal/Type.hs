module Language.Reality.Frontend.Parser.Internal.Type where

import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | TYPE
-- | Parse a type.
parseType :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.Type)
parseType =
    P.choice
        [ -- Function type constructor
          -- Defined as the following:
          --
          -- "fn" "(" type ("," type)* ")" "->" type
          do
            ((start, _), _) <- Lex.reserved "fn"
            tys <- snd <$> Lex.parens (P.sepBy (snd <$> parseType) Lex.comma)
            ((_, end), ret) <- Lex.symbol "->" *> parseType

            pure ((start, end), tys HLIR.:->: ret)
        , -- Pointer type constructor
          -- Defined as the following:
          --
          -- "*" type
          do
            ((start, _), _) <- Lex.symbol "*"
            ((_, end), ty) <- parseType

            pure ((start, end), HLIR.MkTyPointer ty)
        , -- Tuple type constructor
          -- Defined as the following:
          --
          -- "(" type "," type ")"
          do
            (pos, ty) <- Lex.parens $ do
                x <- snd <$> parseType
                void $ Lex.reserved ","
                HLIR.MkTyTuple x . snd <$> parseType
            pure (pos, ty)
        , -- Type application constructor
          -- Defined as the following:
          --
          -- identifier "<" type ("," type)* ">"
          P.try $ do
            ((start, _), idt) <- Lex.identifier
            ((_, end), tys) <- Lex.brackets $ P.sepBy1 (snd <$> parseType) Lex.comma

            pure ((start, end), HLIR.MkTyApp (HLIR.MkTyId idt) tys)
        , Lex.identifier <&> second HLIR.MkTyId
        ]
