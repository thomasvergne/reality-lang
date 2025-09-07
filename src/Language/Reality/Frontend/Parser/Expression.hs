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
        <$> parseLiteralSuffix

    where
        parseLiteralSuffix :: (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
        parseLiteralSuffix = Lex.lexeme $ do
            lit <- Lit.parseLiteral
            suffix <- P.optional $ P.choice [
                  P.string "u8"
                , P.string "u16"
                , P.string "u32"
                , P.string "u64"
                , P.string "i8"
                , P.string "i16"
                , P.string "i32"
                , P.string "i64"
                , P.string "f32"
                , P.string "f64"
                ]

            case suffix of
                Nothing -> pure (HLIR.MkExprLiteral lit)
                Just s -> pure (HLIR.MkExprCast (HLIR.MkExprLiteral lit) (HLIR.MkTyId s))

-- | PARSE IF-IS EXPRESSION
-- | Parse an if-is expression. An if-is expression is an expression that consists
-- | of a condition, a pattern to match, and a then branch. It is used to conditionally
-- | evaluate an expression based on a pattern match.
-- | The syntax of an if-is expression is as follows:
-- |
-- | "if" expression "is" pattern "{" statements "}" ("else" "{" statements "}")?
-- |
parseExprIfIs ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprIfIs = do
    ((start, _), _) <- Lex.reserved "if"

    cond <- snd <$> parseExprTerm
    void $ Lex.reserved "is"
    pattern <- snd <$> parsePatternFull

    ((_, firstEnd), thenBranch) <- parseExprBlock

    result <- P.optional $ do
        void $ Lex.reserved "else"

        ((_, end), elseBranch) <- parseExprFull

        pure (end, elseBranch)

    case result of
        Nothing ->
            pure
                ((start, firstEnd), HLIR.MkExprIfIs cond pattern thenBranch Nothing Nothing)
        Just (end, elseBranch) ->
            pure
                ( (start, end)
                , HLIR.MkExprIfIs cond pattern thenBranch (Just elseBranch) Nothing
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

    ((_, firstEnd), thenBranch) <- parseExprBlock

    result <- P.optional $ do
        void $ Lex.reserved "else"

        ((_, end), elseBranch) <- parseExprFull

        pure (end, elseBranch)

    case result of
        Nothing -> pure ((start, firstEnd), HLIR.MkExprSingleIf cond thenBranch Nothing)
        Just (end, elseBranch) -> pure ((start, end), HLIR.MkExprCondition cond thenBranch elseBranch Nothing)

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

    pure ((start, end), HLIR.MkExprLetIn binding value body Nothing)

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
    buildBlockFromList (HLIR.MkExprLocated p e : xs) =
        HLIR.MkExprLocated p (buildBlockFromList (e : xs))
    buildBlockFromList [x] = x
    buildBlockFromList (HLIR.MkExprLetIn ann v b _ : xs)
        | isUnit b = HLIR.MkExprLetIn ann v (buildBlockFromList xs) Nothing
        | otherwise = HLIR.MkExprLetIn ann v (buildBlockFromList (b : xs)) Nothing
    buildBlockFromList (HLIR.MkExprWhile cond body ty inE : xs)
        | isUnit inE = HLIR.MkExprWhile cond body ty (buildBlockFromList xs)
        | otherwise = HLIR.MkExprWhile cond body ty (buildBlockFromList (inE : xs))
    buildBlockFromList (HLIR.MkExprWhileIs cond pat body ty inE : xs)
        | isUnit inE = HLIR.MkExprWhileIs cond pat body ty (buildBlockFromList xs)
        | otherwise =
            HLIR.MkExprWhileIs cond pat body ty (buildBlockFromList (inE : xs))
    buildBlockFromList (HLIR.MkExprReturn e : _) = HLIR.MkExprReturn e
    buildBlockFromList (HLIR.MkExprBreak : _) = HLIR.MkExprBreak
    buildBlockFromList (HLIR.MkExprContinue : _) = HLIR.MkExprContinue
    buildBlockFromList (x : xs) =
        HLIR.MkExprLetIn
            (HLIR.MkAnnotation "_" Nothing)
            x
            (buildBlockFromList xs)
            Nothing

    isUnit :: HLIR.HLIR "expression" -> Bool
    isUnit (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" _) _) = True
    isUnit (HLIR.MkExprLocated _ e) = isUnit e
    isUnit _ = False

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

-- | PARSE SIZEOF EXPRESSION
-- | Parse a sizeof expression. A sizeof expression is an expression that consists
-- | of the keyword "sizeof" followed by a type in parentheses. It is used to get
-- | the size of a type in bytes in Reality. The syntax of a sizeof expression is
-- | as follows:
-- |
-- | "sizeof" "(" type ")"
parseExprSizeOf ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseExprSizeOf = do
    ((start, _), _) <- Lex.reserved "sizeof"
    ((_, end), ty) <- Lex.parens (snd <$> Typ.parseType)
    pure ((start, end), HLIR.MkExprSizeOf ty)

-- | PARSE PATTERN
-- | Parse a pattern. A pattern is used in match expressions and let bindings
-- | to destructure values. The syntax of a pattern is as follows:
-- |
-- | - identifier
-- | - "_" (wildcard)
-- | - literal
-- | - identifier "(" (pattern ("," pattern)*)? ")"
-- | - "let" identifier
parsePatternFull ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "pattern")
parsePatternFull =
    Lex.locateWith
        <$> P.choice
            [ P.try $ do
                ((start, _), _) <- Lex.reserved "let"
                ((_, end), name) <- Lex.identifier
                pure ((start, end), HLIR.MkPatternLet (HLIR.MkAnnotation name Nothing))
            , P.try $ do
                ((start, _), name) <- Lex.identifier
                ((_, end), args) <- Lex.parens (P.sepBy (snd <$> parsePatternFull) Lex.comma)
                pure ((start, end), HLIR.MkPatternConstructor name args Nothing)
            , do
                (pos, name) <- Lex.identifier
                pure (pos, HLIR.MkPatternVariable (HLIR.MkAnnotation name Nothing))
            , do
                (pos, _) <- Lex.symbol "_"
                pure (pos, HLIR.MkPatternWildcard)
            , do
                Lex.lexeme Lit.parseLiteral
                    <&> second HLIR.MkPatternLiteral
            ]

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
            , parseExprSizeOf
            , P.try parseExprTernary
            , parseExprIfIs
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
        [ -- 1. Postfix: function call (), array indexing [], struct access ., -> (highest)

            [ P.Postfix . Lex.makeUnaryOp $ do
                void $ Lex.symbol "["
                index <- snd <$> parseExprFull
                ((_, end), _) <- Lex.symbol "]"
                pure $ \(start, arr) -> ((fst start, end), HLIR.MkExprVarCall "get_index" [arr, index])
            ]
        ,
            [ P.Postfix . Lex.makeUnaryOp $ do
                ((_, end), field) <- P.string "." *> Lex.nonLexedID <* Lex.scn

                optArgs <- P.optional $ Lex.parens (P.sepBy (snd <$> parseExprFull) Lex.comma)

                pure $ \((start, _), e) -> case optArgs of
                    Nothing -> ((start, end), HLIR.MkExprStructureAccess e field)
                    Just ((_, end'), args) -> ((start, end'), HLIR.MkExprFunctionAccess field e args)
            ]
        ,
            [ P.Postfix . Lex.makeUnaryOp $ do
                ((_, end), args) <-
                    Lex.parens
                        (P.sepBy (snd <$> parseExprFull) Lex.comma)
                pure $ \((start, _), e) -> ((start, end), HLIR.MkExprApplication e args Nothing)
            ]
        ,
            [ P.Postfix . Lex.makeUnaryOp $ do
                void $ Lex.symbol "->"
                ((_, end), field) <- Lex.nonLexedID <* Lex.scn

                optArgs <- P.optional $ Lex.parens (P.sepBy (snd <$> parseExprFull) Lex.comma)

                pure $ \((start, _), e) ->
                    case optArgs of
                        Nothing ->
                            ( (start, end)
                            , HLIR.MkExprStructureAccess (HLIR.MkExprDereference e Nothing) field
                            )
                        Just ((_, end'), args) ->
                            ( (start, end')
                            , HLIR.MkExprFunctionAccess field (HLIR.MkExprDereference e Nothing) args
                            )
            ]
        , -- 2. Prefix/unary: *, &, cast, etc.

            [ P.Prefix . Lex.makeUnaryOp $ do
                void $ Lex.symbol "*"
                pure $ second (`HLIR.MkExprDereference` Nothing)
            , P.Prefix . Lex.makeUnaryOp $ do
                void $ Lex.symbol "&"
                pure $ second (`HLIR.MkExprReference` Nothing)
            ]
        ,
            [ P.Postfix . Lex.makeUnaryOp $ do
                void $ Lex.reserved "as"
                ((_, end), ty) <- Typ.parseType
                pure $ \((start, _), e) -> ((start, end), HLIR.MkExprCast e ty)
            ]
        , -- 3. Multiplicative: *, /, %

            [ P.InfixL $ do
                void $ Lex.symbol "*"
                pure $ makeOperator "mul"
            , P.InfixL $ do
                void $ Lex.symbol "/"
                pure $ makeOperator "div"
            , P.InfixN $ do
                void $ Lex.symbol "%"
                pure $ makeOperator "modulo"
            ]
        , -- 4. Additive: +, -

            [ P.InfixL $ do
                void $ Lex.symbol "+"
                pure $ makeOperator "add"
            , P.InfixL $ do
                void $ Lex.symbol "-"
                pure $ makeOperator "sub"
            ]
        , -- 5. Relational: <, >, <=, >=

            [ P.InfixN $ do
                void $ Lex.symbol ">="
                pure $ makeOperator "great_equals"
            , P.InfixN $ do
                void $ Lex.symbol "<="
                pure $ makeOperator "less_equals"
            , P.InfixN $ do
                void $ Lex.symbol ">"
                pure $ makeOperator "greater"
            , P.InfixN $ do
                void $ Lex.symbol "<"
                pure $ makeOperator "lesser"
            ]
        , -- 6. Equality: ==, !=

            [ P.InfixN $ do
                void $ Lex.symbol "=="
                pure $ makeOperator "equals"
            , P.InfixN $ do
                void $ Lex.symbol "!="
                pure $ makeOperator "not_equals"
            ]
        , -- 7. Logical AND: &&

            [ P.InfixL $ do
                void $ Lex.symbol "&&"
                pure $ makeOperator "and"
            ]
        , -- 8. Logical OR: ||

            [ P.InfixL $ do
                void $ Lex.symbol "||"
                pure $ makeOperator "or"
            ]
        , -- 9. Assignment: =

            [ P.InfixR $ do
                void $ Lex.symbol "="
                pure $ \((start, _), a) ((_, end), b) -> ((start, end), HLIR.MkExprUpdate a b Nothing)
            ]
        , -- 10. (Optional) Exponentiation: **

            [ P.InfixL $ do
                void $ Lex.symbol "**"
                pure $ makeOperator "pow"
            ]
        ]

parseStmtFull ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtFull = do
    Lex.locateWith
        <$> P.choice
            [ parseStmtLet
            , P.try parseStmtWhile
            , parseStmtWhileIs
            , parseStmtReturn
            , parseStmtBreak
            , parseStmtContinue
            , parseExprFull
            ]

-- | PARSE RETURN STATEMENT
-- | Parse a return statement. A return statement is a statement that consists
-- | of the keyword "return" followed by an expression. It is used to return a value
-- | from a function in Reality. The syntax of a return statement is as follows:
-- |
-- | "return" expression
parseStmtReturn ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtReturn = do
    ((start, _), _) <- Lex.reserved "return"

    ((_, end), expr) <- parseExprFull

    pure ((start, end), HLIR.MkExprReturn expr)

-- | PARSE BREAK STATEMENT
-- | Parse a break statement. A break expression is a statement that consists
-- | of the keyword "break". It is used to exit a loop in Reality. The syntax of
-- | a break statement is as follows:
-- |
-- | "break"
parseStmtBreak ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtBreak = do
    ((start, end), _) <- Lex.reserved "break"
    pure ((start, end), HLIR.MkExprBreak)

-- | PARSE CONTINUE EXPRESSION
-- | Parse a continue statement. A continue statement is a statement that consists
-- | of the keyword "continue". It is used to skip the current iteration of a loop
-- | in Reality. The syntax of a continue statement is as follows:
-- |
-- | "continue"
parseStmtContinue ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtContinue = do
    ((start, end), _) <- Lex.reserved "continue"
    pure ((start, end), HLIR.MkExprContinue)

-- | PARSE WHILE-IS STATEMENT
-- | Parse a while-is statement. A while-is expression is an expression that consists
-- | of a condition, a pattern to match, and a body. It is used to repeatedly evaluate
-- | an expression based on a pattern match.
-- | The syntax of a while-is expression is as follows:
-- |
-- | "while" expression "is" pattern "{" statements "}"
-- |
parseStmtWhileIs ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtWhileIs = do
    ((start, _), _) <- Lex.reserved "while"

    cond <- snd <$> parseExprTerm
    void $ Lex.reserved "is"
    pattern <- snd <$> parsePatternFull

    ((_, end), body) <- parseExprBlock

    pure
        ( (start, end)
        , HLIR.MkExprWhileIs
            cond
            pattern
            body
            Nothing
            (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing) [])
        )

parseStmtWhile ::
    (MonadIO m) => P.Parser m (HLIR.Position, HLIR.HLIR "expression")
parseStmtWhile = do
    ((start, _), _) <- Lex.reserved "while"
    cond <- snd <$> parseExprFull
    ((_, end), body) <- parseExprBlock

    pure
        ( (start, end)
        , HLIR.MkExprWhile
            cond
            body
            Nothing
            (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing) [])
        )

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
            Nothing
        )
