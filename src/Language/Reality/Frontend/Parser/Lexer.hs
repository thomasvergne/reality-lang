module Language.Reality.Frontend.Parser.Lexer where

import Data.Char
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Syntax.HLIR (Locate (..), Position)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Prelude hiding (modify)

{-# NOINLINE defaultPosition #-}
defaultPosition :: IORef (Maybe Position)
defaultPosition = IO.unsafePerformIO $ newIORef Nothing

-- LEXING FUNCTIONS

-- | Make a unary operator sequence
-- | A unary operator sequence is a sequence of unary operators
-- | that are applied to an expression.
-- |
-- | example: not not a <=> not (not a)
-- |
-- | The sequence is applied from right to left.
-- | This is because the postfix and prefix operators are applied *
-- | from right to left.
makeUnaryOp :: (Alternative f) => f (a -> a) -> f (a -> a)
makeUnaryOp s = List.foldr1 (.) . reverse <$> some s

-- | Skip inline comments
-- | Inline comments are comments that start with // and end at the end
-- | of the line.
lineComment :: (MonadIO m) => P.Parser m ()
lineComment = L.skipLineComment "//"

-- | Skip block comments
-- | Block comments are comments that start with /* and end with */
-- | Block comments can span multiple lines
-- | Block comments are also nestable
multilineComment :: (MonadIO m) => P.Parser m ()
multilineComment = L.skipBlockComment "/*" "*/"

-- | Describe how to treat whitespace and comments
scn :: (MonadIO m) => P.Parser m ()
scn = L.space space1 lineComment multilineComment

-- | Describe the reserved keywords and primitive of the language
-- | It is used to tell the identifier parser what is a reserved keyword
-- | and what should an identifier not be.
reservedWords :: Set Text
reservedWords =
    Set.fromList
        [ "mod"
        , "let"
        , "import"
        , "fn"
        , "pub"
        , "const"
        , "in"
        , "if"
        , "else"
        , "extern"
        , "is"
        ]

-- | Lexeme parser that consumes whitespace after the lexeme
-- | This is an utility function that converts a non-lexeme parser
-- | into a lexeme parser
lexeme :: (MonadIO m) => P.Parser m a -> P.Parser m (Position, a)
lexeme p = L.lexeme scn $ do
    start <- P.getSourcePos
    r <- p
    end <- P.getSourcePos
    return ((start, end), r)

-- | Parse a symbol and consume whitespace after the symbol
symbol :: (MonadIO m) => Text -> P.Parser m (Position, Text)
symbol = lexeme . L.symbol mempty

-- | Describe the valid operators in the language
-- | Any other operator is ovbiously invalid
-- | This may be used with `some` to create an operator composed
-- | of valid operators, such as `<+>`, `<!>`, `<?>`
validOperators :: Set Char
validOperators =
    Set.fromList
        [ '!'
        , '#'
        , '$'
        , '%'
        , '&'
        , '*'
        , '+'
        , '.'
        , '/'
        , '<'
        , '='
        , '>'
        , '?'
        , '@'
        , '^'
        , '|'
        , '-'
        , '~'
        ]

-- | Describe the reserved operators in the language
-- | Reserved operators are operators that are used in the language
-- | and cannot be used as custom operators
-- | This is used to prevent the user from defining operators that
-- | may break the parser (for instance `=>` is used in lambda expressions)
reservedOperators :: Set Text
reservedOperators =
    Set.fromList
        [".", "..", "=>", ","]

-- | Parse a sequence of valid operators, checking if they are valid
-- | and not reserved, and then return the concatenated operator
operator :: (MonadIO m) => P.Parser m (Position, Text)
operator = P.try $ do
    op <- lexeme (P.takeWhile1P Nothing (`Set.member` validOperators))
    guard (snd op `Set.notMember` reservedOperators)
    return op

-- | Parse a reserved keyword
-- | A reserved keyword is a keyword that is used in the language
-- | and cannot be used as an identifier. For instance `if`, `else`, `return`
-- | are reserved keywords. It should not be followed by an alphanumeric
-- | character: `if` is a reserved keyword, but `iff` is not.
reserved :: (MonadIO m) => Text -> P.Parser m (Position, Text)
reserved keyword = P.try $ lexeme (string keyword <* P.notFollowedBy alphaNumChar)

-- BASIC LEXEME PARSERS

parens :: (MonadIO m) => P.Parser m a -> P.Parser m (Position, a)
parens p = do
    ((start, _), _) <- symbol "("
    r <- p
    ((_, end), _) <- symbol ")"
    return ((start, end), r)

brackets :: (MonadIO m) => P.Parser m a -> P.Parser m (Position, a)
brackets p = do
    ((start, _), _) <- symbol "["
    r <- p
    ((_, end), _) <- symbol "]"
    return ((start, end), r)

angles :: (MonadIO m) => P.Parser m a -> P.Parser m (Position, a)
angles p = do
    ((start, _), _) <- symbol "<"
    r <- p
    ((_, end), _) <- symbol ">"
    return ((start, end), r)

braces :: (MonadIO m) => P.Parser m a -> P.Parser m (Position, a)
braces p = do
    ((start, _), _) <- symbol "{"
    r <- p
    ((_, end), _) <- symbol "}"
    return ((start, end), r)

comma :: (MonadIO m) => P.Parser m Text
comma = snd <$> symbol ","

colon :: (MonadIO m) => P.Parser m Text
colon = snd <$> symbol ":"

semi :: (MonadIO m) => P.Parser m Text
semi = snd <$> symbol ";"

-- | Check if the character is a valid identifier character
-- | An identifier character is an alphanumeric character or an underscore
-- | For instance, `'test` is not a valid identifier, but `test` is.
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

-- | Check if the character is a valid identifier start character
-- | An identifier start character is an alphabetic character or an underscore
-- | For instance, `'test` is not a valid identifier, but `test` is.
-- | `1` is not a valid identifier too.
isIdentCharStart :: Text -> Bool
isIdentCharStart cs = isAlpha (Text.head cs) || Text.head cs == '_'

isIdentCharStart' :: Char -> Bool
isIdentCharStart' c = isAlpha c || c == '_'

-- | Parse a non-lexed identifier
-- | A non-lexed identifier is an identifier that is not lexed, meaning that
-- | it does not consume whitespace after and before the identifier. This
-- | is useful for parsing record selections.
nonLexedID :: (MonadIO m) => P.Parser m (Position, Text)
nonLexedID = do
    start <- P.getSourcePos
    r <- P.takeWhile1P Nothing isIdentChar
    end <- P.getSourcePos
    -- Guarding parsed result and failing when reserved word is parsed
    -- (such as reserved keyword)
    if r `Set.member` reservedWords
        then fail $ "The identifier " ++ show r ++ " is a reserved word"
        else
            if isIdentCharStart r
                then return ((start, end), r)
                else fail $ "The identifier " ++ show r ++ " is not valid"

-- | Parse an identifier
-- | An identifier is a sequence of valid identifier characters
-- | that starts with an identifier start character
-- | An identifier cannot be a reserved keyword
identifier :: (MonadIO m) => P.Parser m (Position, Text)
identifier = lexeme $ do
    -- Parse identifier segments separated by "::"
    firstSegment <- identifierSegment
    restSegments <- P.many (P.try (string "::" *> identifierSegment))

    let cs = Text.intercalate "::" (firstSegment : restSegments)

    if cs `Set.member` reservedWords
        then fail $ "The identifier " ++ show cs ++ " is a reserved word"
        else return cs
  where
    identifierSegment =
        P.lookAhead (P.satisfy isIdentCharStart')
            *> P.takeWhile1P (Just "identifier") isIdentChar

-- | A field may be either a non-lexed identifier or an operator
field :: (MonadIO m) => P.Parser m (Position, Text)
field = nonLexedID <|> operator

-- | Check if the given number is an integer, meaning that
-- | the number is equal to its floor and ceiling
-- | For instance 1.0 is an integer because floor(1.0) = 1 = ceiling(1.0)
-- | But 1.6 is not an integer because floor(1.6) = 1 != ceiling(1.6)
isInteger :: (Num a, RealFrac a) => a -> Bool
isInteger x = (ceiling x :: Integer) == floor x

locateWith :: (Locate a) => (Position, a) -> (Position, a)
locateWith (p, x) = (p, locate x p)
