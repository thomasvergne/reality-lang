module Language.Reality.Frontend.Parser.Expression where

import Control.Monad.Combinators.Expr qualified as P
import Data.Map qualified as Map
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Internal.Literal qualified as Lit
import Language.Reality.Frontend.Parser.Internal.Type qualified as Typ
import Language.Reality.Frontend.Parser.Lexer qualified as Lex
import Language.Reality.Syntax.HLIR qualified as HLIR
import Text.Megaparsec.Char qualified as P

-- | PARSE ANNOTATION
-- | Parse an annotation. An annotation is used to attach metadata to an AST node.
-- | In this context, an annotation is an identifier followed by an optional type,
-- | consisting of the following:
-- |
-- | - name (":" type)?
parseAnnotation ::
    (MonadIO m) => P.Parser m a -> P.Parser m (HLIR.Annotation (Maybe a))
parseAnnotation p =
    P.choice
        [ P.try $ do
            name <- snd <$> Lex.identifier
            ty <- P.optional (Lex.symbol ":" *> p)

            pure $ HLIR.MkAnnotation name ty
        , HLIR.MkAnnotation . snd <$> Lex.identifier <*> pure Nothing
        ]

-- | PARSE ANNOTATION'
-- | Parse an annotation. An annotation is used to attach metadata to an AST node.
-- | In this context, an annotation is an identifier followed by a type,
-- | consisting of the following:
-- |
-- | - name ":" type
parseAnnotation' ::
    (MonadIO m) => P.Parser m a -> P.Parser m (HLIR.Annotation a)
parseAnnotation' p = HLIR.MkAnnotation . snd <$> Lex.identifier <*> (Lex.symbol ":" *> p)

-- | PARSE LITERAL
-- | Parsing a literal is just parsing a literal value except string literal, which
-- | is covered by the parseInterpolatedString function (used to parse interpolated
-- | strings).
parseExprLiteral ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprLiteral =
    Lex.locateWith
        <$> Lex.lexeme
            ( Lit.parseLiteral
                <&> HLIR.MkExprLiteral
            )

-- | PARSE TERNARY
-- | Parse a ternary expression. A ternary expression is an expression that consists
-- | of three parts: a condition, a then branch, and an else branch. It is used to
-- | conditionally evaluate an expression based on a condition.
-- | The syntax of a ternary expression is as follows:
-- |
-- | "if" expression "{" statements "}" "else" "{" statements "}"
parseExprTernary ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprTernary = do
    ((start, _), _) <- Lex.reserved "if"
    cond <- snd <$> parseExprFull

    (_, thenBranch) <- parseExprBlock

    void $ Lex.reserved "else"

    ((_, end), elseBranch) <- parseExprFull

    pure ((start, end), HLIR.MkExprCondition cond thenBranch elseBranch)

-- | PARSE VARIABLE
-- | Parse a variable expression. A variable expression is an expression that
-- | consists of a variable name. It is used to represent a variable in Reality.
-- | The syntax of a variable expression is as follows:
-- |
-- | identifier
parseExprVariable ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprVariable = do
    (pos@(start, _), name) <- Lex.identifier
    ((_, end), as_type) <-
        P.try . P.option (pos, [])
            $ Lex.symbol "::" *> Lex.brackets ((snd <$> Typ.parseType) `P.sepBy1` Lex.comma)

    pure
        ((start, end), HLIR.MkExprVariable (HLIR.MkAnnotation name Nothing) as_type)

-- | PARSE LET DECLARATION
-- | Parse a let declaration. A let declaration is used to declare a variable
-- | and assign a value to it. It is used to bind a value to a variable in Reality.
-- | The syntax of a let declaration is as follows:
-- |
-- | "let" identifier "=" expression "in" expression
parseExprLetIn ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprLetIn = do
    ((start, _), _) <- Lex.reserved "let"

    binding <- parseAnnotation (snd <$> Typ.parseType)

    void $ Lex.symbol "="

    value <- snd <$> parseExprFull

    void $ Lex.reserved "in"

    ((_, end), body) <- parseExprFull

    pure ((start, end), HLIR.MkExprLetIn binding value body)

-- | PARSE BLOCK EXPRESSION
-- | Parse a block expression. A block expression is an expression that consists of
-- | a block of code. It is used to group a set of expressions together in Reality.
-- | The syntax of a block expression is as follows:
-- |
-- | "{" expression* "}"
parseExprBlock ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprBlock = do
    ((start, _), _) <- Lex.symbol "{"
    exprs <- map snd <$> P.sepEndBy parseStmtFull (P.optional (Lex.symbol ";"))
    ((_, end), _) <- Lex.symbol "}"

    pure ((start, end), buildBlockFromList exprs)
  where
    buildBlockFromList :: [HLIR.HLIR "expression"] -> HLIR.HLIR "expression"
    buildBlockFromList [] = HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing) []
    buildBlockFromList [x] = x
    buildBlockFromList (HLIR.MkExprLocated p e : xs) =
        HLIR.MkExprLocated p (buildBlockFromList (e : xs))
    buildBlockFromList (x : xs) =
        HLIR.MkExprLetIn (HLIR.MkAnnotation "_" Nothing) x (buildBlockFromList xs)

-- | PARSE LAMBDA EXPRESSION
-- | Parse a lambda expression. A lambda expression is an expression that consists
-- | of a set of parameters and a body. It is used to define anonymous functions
-- | in Reality. The syntax of a lambda expression is as follows:
-- |
-- | "|" (identifier (":" type)? ("," identifier (":" type)?)*)? "|" "->" type "{" statements "}"
parseExprLambda ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprLambda = do
    ((start, _), _) <- Lex.symbol "|"

    params <- P.sepBy (parseAnnotation (snd <$> Typ.parseType)) Lex.comma

    void $ Lex.symbol "|"

    returnType <- P.optional $ Lex.symbol "->" *> (snd <$> Typ.parseType)

    ((_, end), body) <- case returnType of
        Just _ -> parseExprBlock
        Nothing -> parseExprFull

    pure ((start, end), HLIR.MkExprLambda params returnType body)

-- | PARSE STRUCTURE CREATION
-- | Parse a structure creation expression. A structure creation expression is an
-- | expression that consists of a structure name and a set of fields. It is used
-- | to create a new instance of a structure in Reality. The syntax of a structure
-- | creation expression is as follows:
-- |
-- | type "{" (identifier ":" expression ("," identifier ":" expression)*)? "}"
parseExprStructCreation ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprStructCreation = do
    ((start, _), header) <- Typ.parseType

    ((_, end), fields) <- Lex.braces $ Map.fromList <$> P.sepBy parseField Lex.comma

    pure ((start, end), HLIR.MkExprStructureCreation header fields)
  where
    parseField :: (MonadIO m) => P.Parser m (Text, HLIR.HLIR "expression")
    parseField = do
        name <- snd <$> Lex.identifier

        void $ Lex.symbol ":"

        value <- snd <$> parseExprFull

        pure (name, value)

-- | PARSE TERM EXPRESSION
-- | Parse a term expression. A term expression is the most basic form of an expression
-- | in Reality. It can be a literal, a variable, a parenthesized expression, a block,
-- | a lambda, a structure creation, or a ternary expression.
-- | The syntax of a term expression is as follows:
-- |
-- | - literal
-- | - variable
-- | - "(" expression ")"
-- | - block
-- | - lambda
-- | - structure creation
-- | - ternary
-- | - let-in
parseExprTerm ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprTerm =
    Lex.locateWith
        <$> P.choice
            [ snd <$> Lex.parens parseExprFull
            , parseExprBlock
            , parseExprLambda
            , parseExprTernary
            , P.try parseExprStructCreation
            , parseExprLetIn
            , parseExprLiteral
            , parseExprVariable
            ]

makeOperator ::
    Text ->
    (HLIR.Position, HLIR.HLIR "expression") ->
    (HLIR.Position, HLIR.HLIR "expression") ->
    (HLIR.Position, HLIR.HLIR "expression")
makeOperator op ((start, _), a) ((_, end), b) =
    ( (start, end)
    , HLIR.MkExprBinary
        op
        a
        b
    )

-- | PARSE FULL EXPRESSION
-- | Parse a full expression. A full expression is an expression that can contain
-- | operators and function calls. It is used to represent complex expressions in
-- | Reality. The syntax of a full expression is as follows:
-- |
-- | - term (operator term)*
-- | - term "(" (expression ("," expression)*)? ")"
-- | - term "[" expression "]"
-- | - term "." identifier
-- |
-- | The operators are defined in the operators list below, with their precedence
-- | and associativity.
-- | The precedence levels are as follows (from highest to lowest):
-- |
-- | 1. Function call, array indexing, structure field access (left associative)
-- | 2. Multiplication, division (left associative)
-- | 3. Addition, subtraction (left associative)
-- | 4. Relational operators (non-associative)
-- | 5. Equality operators (non-associative)
-- | 6. Logical AND (left associative)
-- | 7. Logical OR (left associative)
-- |
-- | Unary operators (right associative)
-- | The associativity of the operators is defined as follows:
-- |
-- | - Left associative: a op b op c = (a op b) op c
-- | - Right associative: a op b op c = a op (b op c)
parseExprFull ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprFull = Lex.locateWith <$> P.makeExprParser parseExprTerm operators
  where
    operators =
        [
            [ P.Postfix . Lex.makeUnaryOp $ do
                void $ Lex.symbol "["
                index <- snd <$> parseExprFull
                ((_, end), _) <- Lex.symbol "]"

                pure $ \(start, arr) -> ((fst start, end), HLIR.MkExprVarCall "get_index" [arr, index])
            ]
        ,
            [ P.Postfix . Lex.makeUnaryOp $ do
                ((_, end), field) <- P.string "." *> Lex.nonLexedID <* Lex.scn
                pure $ \((start, _), e) -> ((start, end), HLIR.MkExprStructureAccess e field)
            ]
        ,
            [ P.Postfix . Lex.makeUnaryOp $ do
                ((_, end), args) <- Lex.parens (P.sepBy (snd <$> parseExprFull) Lex.comma)

                pure $ \((start, _), e) -> ((start, end), HLIR.MkExprApplication e args)
            ]
        ,
            [ P.InfixL $ do
                void $ Lex.symbol "*"
                pure $ makeOperator "*"
            , P.InfixL $ do
                void $ Lex.symbol "/"
                pure $ makeOperator "/"
            ]
        ,
            [ P.InfixL $ do
                void $ Lex.symbol "+"
                pure $ makeOperator "+"
            , P.InfixL $ do
                void $ Lex.symbol "-"
                pure $ makeOperator "-"
            ]
        ,
            [ P.InfixN $ do
                void $ Lex.symbol "=="
                pure $ makeOperator "=="
            , P.InfixN $ do
                void $ Lex.symbol "!="
                pure $ makeOperator "!="
            ]
        ,
            [ P.InfixN $ do
                void $ Lex.symbol ">="
                pure $ makeOperator ">="
            , P.InfixN $ do
                void $ Lex.symbol "<="
                pure $ makeOperator "<="
            , P.InfixN $ do
                void $ Lex.symbol ">"
                pure $ makeOperator ">"
            , P.InfixN $ do
                void $ Lex.symbol "<"
                pure $ makeOperator "<"
            ]
        ,
            [ P.InfixL $ do
                void $ Lex.symbol "||"
                pure $ makeOperator "||"
            ]
        ,
            [ P.InfixL $ do
                void $ Lex.symbol "&&"
                pure $ makeOperator "&&"
            ]
        ]

parseStmtFull ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtFull = do
    P.choice
        [ parseStmtLet
        , parseExprFull
        ]

parseStmtLet ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtLet = do
    ((start, _), _) <- Lex.reserved "let"
    binding <- parseAnnotation (snd <$> Typ.parseType)
    void $ Lex.symbol "="
    ((_, end), value) <- parseExprFull

    pure
        ( (start, end)
        , HLIR.MkExprLetIn
            binding
            value
            (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing) [])
        )
