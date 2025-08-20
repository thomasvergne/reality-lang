use std::collections::HashMap;

use reality_ast::{build_block_from_statements, internal::{annotation::Annotation, literal::Literal, types::Type}, unit, ASTNode, ToplevelNode};
use reality_error::RealityError;

#[derive(Debug, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    NonAssociative,
}

type Position = (usize, usize);
type Result<T> = std::result::Result<(T, Position), RealityError>;

#[derive(Debug, Clone, PartialEq)]
pub enum OperatorType {
    Infix(fn(&ASTNode, &ASTNode) -> std::result::Result<ASTNode, RealityError>),
    Prefix(fn(&ASTNode) -> std::result::Result<ASTNode, RealityError>),
    Postfix(fn(&ASTNode) -> std::result::Result<ASTNode, RealityError>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser<'a> {
    pub input: &'a str,
    pub position: usize,
    pub file: &'a str,
    operators: HashMap<&'a str, (Associativity, usize, OperatorType)>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, file: &'a str) -> Self {
        Parser {
            input,
            position: 0,
            file,
            operators: HashMap::new(),
        }
    }

    pub fn add_operator(
        &mut self,
        name: &'a str,
        associativity: Associativity,
        precedence: usize,
        operator_type: OperatorType,
    ) {
        self.operators
            .insert(name, (associativity, precedence, operator_type));
    }

    fn skip_whitespaces(&mut self) {
        while self.position < self.input.len()
            && self.input[self.position..].starts_with(char::is_whitespace)
        {
            self.position += 1;
        }
    }

    fn expect_token(&mut self, token: &'a str) -> Result<&'a str> {
        self.skip_whitespaces();
        let start = self.position;
        if self.input[self.position..].starts_with(token) {
            self.position += token.len();
            Ok((token, (start, self.position)))
        } else {
            Err(RealityError::ExpectedToken(token.to_string()))
        }
    }

    fn is_reserved(&self, input: &'a str) -> bool {
        matches!(input, "let" | "in" | "if" | "else" | "fn")
    }

    fn consume_token(&mut self, token: &'a str) -> Result<&'a str> {
        self.skip_whitespaces();
        let start = self.position;
        if self.peek_token(token) {
            self.position += token.len();
            self.skip_whitespaces();
            Ok((token, (start, self.position)))
        } else {
            Err(RealityError::ExpectedToken(token.to_string()))
        }
    }

    fn peek_token(&self, token: &'a str) -> bool {
        self.input[self.position..].starts_with(token)
    }

    pub fn parse_program(&mut self) -> std::result::Result<Vec<ToplevelNode>, RealityError> {
        let mut nodes = Vec::new();

        while self.position < self.input.len() {
            let (node, _) = self.parse_toplevel()?;

            if self.peek_token(";") {
                self.position += 1;
            }

            self.skip_whitespaces();

            nodes.push(node);
        }

        Ok(nodes)
    }

    fn parse_toplevel(&mut self) -> Result<ToplevelNode> {
        self.skip_whitespaces();

        if self.position >= self.input.len() {
            return Err(RealityError::UnexpectedEndOfFile);
        }

        if self.peek_token("const") {
            return self.parse_top_constant_decl();
        } else if self.peek_token("fn") {
            return self.parse_top_function();
        } else if self.peek_token("import") {
            return self.parse_top_require();
        } else if self.peek_token("type") {
            return self.parse_top_type_alias();
        }

        Err(RealityError::ExpectedToken("<toplevel>".to_string()))
    }

    fn parse_type_application(&mut self) -> Result<Type> {
      self.skip_whitespaces();
      let start = self.position;

      let (identifier, _) = self.parse_identifier()?;

      self.skip_whitespaces();
      if self.peek_token("(") {
        self.position += 1;
        let mut arguments = Vec::new();

        while !self.peek_token(")") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let (arg, _) = self.parse_type()?;
            arguments.push(arg);
            self.skip_whitespaces();
            if self.peek_token(",") {
                self.position += 1; // Skip the comma
            } else if !self.peek_token(")") {
                return Err(RealityError::ExpectedToken(":".to_string()));
            }
        }

        self.position += 1; // Skip the closing parenthesis
        self.skip_whitespaces();

        return Ok((
            Type::TypeApplication(
                Box::new(Type::TypeIdentifier(identifier.to_string())),
                arguments,
            ),
            (start, self.position),
        ))
      }

      Ok((
          Type::TypeIdentifier(identifier.to_string()),
          (start, self.position),
      ))
    }

    fn parse_top_function(&mut self) -> Result<ToplevelNode> {
        self.consume_token("fn")?;

        let (name, (start_pos, end_pos_1)) = self.parse_identifier()?;

        let mut end_pos = end_pos_1;

        let mut generics = Vec::new();

        if self.peek_token("[") {
            self.consume_token("[")?;
            while !self.peek_token("]") {
                self.skip_whitespaces();
                if self.position >= self.input.len() {
                    return Err(RealityError::UnexpectedEndOfFile);
                }
                let (generic, _) = self.parse_identifier()?;
                generics.push(generic.to_string());
                self.skip_whitespaces();
                if self.peek_token(",") {
                    self.position += 1; // Skip the comma
                }
            }

            let (_, (_, end_pos_2)) = self.consume_token("]")?;

            end_pos = end_pos_2;
        }

        self.consume_token("(")?;
        let mut parameters = Vec::new();

        while !self.peek_token(")") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let (param, (start, _)) = self.parse_identifier()?;
            
            self.consume_token(":")?;

            let (param_type, (_, end)) = self.parse_type()?;

            parameters.push(Annotation {
                name: param.to_string(),
                value: param_type,
                location: (start, end),
            });
            
            self.skip_whitespaces();
            if self.peek_token(",") {
                self.position += 1; // Skip the comma
            }
        }

        let mut return_type = Type::TypeIdentifier("unit".to_string());
        
        if self.peek_token("->") {
            self.consume_token("->")?;
            let (ty, _) = self.parse_type()?;

            return_type = ty;
        }

        let (body, _) = self.parse_block_expression()?;

        Ok((
            ToplevelNode::FunctionDeclaration {
                name: Annotation {
                    name: name.to_string(),
                    value: generics,
                    location: (start_pos, end_pos),
                },
                parameters,
                return_type,
                body: Box::new(body),
            },
            (start_pos, end_pos),
        ))
    }

    fn parse_top_require(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("import")?;

        let (module, (_, end_pos)) = self.parse_literal_string('"')?;

        Ok((
            ToplevelNode::ImportDeclaration(module),
            (start_pos, end_pos),
        ))
    }

    fn parse_top_type_alias(&mut self) -> Result<ToplevelNode> {
        self.consume_token("type")?;

        let (name, (start_pos, end_pos_1)) = self.parse_identifier()?;

        let mut end_pos = end_pos_1;
        let mut generics = Vec::new();
        
        if self.peek_token("[") {
            self.consume_token("[")?;
            while !self.peek_token("]") {
                self.skip_whitespaces();
                if self.position >= self.input.len() {
                    return Err(RealityError::UnexpectedEndOfFile);
                }
                let (generic, _) = self.parse_identifier()?;
                generics.push(generic.to_string());
                self.skip_whitespaces();
                if self.peek_token(",") {
                    self.position += 1; // Skip the comma
                } else if !self.peek_token("]") {
                    return Err(RealityError::ExpectedToken("]".to_string()));
                }
            }
            let (_, (_, end_pos_2)) = self.consume_token("]")?;

            end_pos = end_pos_2;
        }

        self.consume_token("=")?;

        let (body, _) = self.parse_type()?;

        Ok((
            ToplevelNode::TypeAlias {
                name: Annotation {
                    name: name.to_string(),
                    value: generics,
                    location: (start_pos, end_pos),
                },
                body,
            },
            (start_pos, end_pos),
        ))
    }

    fn parse_top_constant_decl(&mut self) -> Result<ToplevelNode> {
        self.consume_token("const")?;

        let (name, (start_pos, end_pos_1)) = self.parse_identifier()?;

        let mut end_pos = end_pos_1;

        let mut annotation = None;

        if self.peek_token(":") {
            self.consume_token(":")?;
            let (type_annotation, (_, end_pos_2)) = self.parse_type()?;
            annotation = Some(type_annotation);

            end_pos = end_pos_2;
        }

        self.consume_token("=")?;

        let (value, _) = self.parse_expression(0)?;

        Ok((ToplevelNode::ConstantDeclaration {
            variable: Annotation {
                name: name.to_string(),
                value: annotation,
                location: (start_pos, end_pos)
            },
            value: Box::new(value),
        }, (start_pos, end_pos)))
    }

    fn parse_type_pointer(&mut self) -> Result<Type> {
        let (_, (start, _)) = self.consume_token("*")?;
        let (pointed_type, (_, end)) = self.parse_type()?;
        Ok((Type::TypeApplication(
            Box::new(Type::TypeIdentifier("pointer".to_string())),
            vec![pointed_type]
        ), (start, end)))
    }

    fn parse_type_function(&mut self) -> Result<Type> {
        self.skip_whitespaces();
        let start = self.position;
        self.consume_token("fn")?;
        self.consume_token("(")?;

        let mut arguments = Vec::new();

        while !self.peek_token(")") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let (arg, _) = self.parse_type()?;
            arguments.push(arg);
            self.skip_whitespaces();
            if self.peek_token(",") {
                self.position += 1; // Skip the comma
            }
        }

        self.consume_token("->")?;
        let (return_type, (_, end)) = self.parse_type()?;

        Ok((
            Type::TypeFunction {
                parameters: arguments,
                return_type: Box::new(return_type),
            },
            (start, end),
        ))
    }

    fn parse_type(&mut self) -> Result<Type> {
        if self.peek_token("fn") {
            return self.parse_type_function();
        } else if self.peek_token("*") {
            return self.parse_type_pointer();
        }

        return self.parse_type_application();
    }

    fn parse_term(&mut self) -> Result<ASTNode> {
        if self.position >= self.input.len() {
            return Err(RealityError::UnexpectedEndOfFile);
        }

        if self.peek_token("'") {
            let (string_literal, pos) = self.parse_literal_string('\'')?;
            return Ok((
                ASTNode::Literal(Literal::String(string_literal)).located(pos),
                pos,
            ));
        } else if self.peek_token("\"") {
            let (string_literal, pos) = self.parse_literal_string('"')?;
            return Ok((
                ASTNode::Literal(Literal::String(string_literal)).located(pos),
                pos,
            ));
        } else if self.peek_token("true") || self.peek_token("false") {
            let (literal, pos) = self.parse_literal_boolean()?;
            return Ok((ASTNode::Literal(Literal::Boolean(literal)).located(pos), pos));
        } else if self.input[self.position..]
            .chars()
            .next()
            .unwrap()
            .is_numeric()
        {
            let (literal, pos) = self.parse_literal_number()?;
            return Ok((ASTNode::Literal(literal).located(pos), pos));
        } else if let Ok((call, pos)) = self.parse_call_expression() {
            return Ok((call.located(pos), pos));
        } else if self.peek_token("if") {
            return self.parse_if_expression();
        } else if self.peek_token("let") {
            return self.parse_let_declaration();
        } else if self.peek_token("{") {
            return self.parse_block_expression();
        } else if self.peek_token(".") {
            return self
                .parse_literal_float_prefixed()
                .map(|(lit, pos)| (ASTNode::Literal(lit).located(pos), pos));
        } else if self.peek_token("|") {
            return self.parse_lambda();
        }

        Err(RealityError::ExpectedToken("term".to_string()))
    }

    fn parse_let_declaration(&mut self) -> Result<ASTNode> {
        self.consume_token("let")?;

        let (name, pos) = self.parse_identifier()?;
        self.skip_whitespaces();
        self.consume_token("=")?;
        let (value, _) = self.parse_expression(0)?;
        self.skip_whitespaces();

        Ok((
            ASTNode::LetIn {
                variable: Annotation {
                    name: name.to_string(),
                    value: None,
                    location: pos,
                },
                value: Box::new(value),
                body: Box::new(unit()),
            }
            .located(pos),
            pos,
        ))
    }

    fn parse_lambda(&mut self) -> Result<ASTNode> {
        let (_, (start_pos,  _)) = self.consume_token("|")?;
        let (parameters, _) = self.parse_parameters("|")?;
        
        let mut return_type = None;

        if self.peek_token(":") {
            self.consume_token(":")?;

            let (ty, _) = self.parse_type()?;

            return_type = Some(ty);
        }

        let (body, (_, end_post)) = self.parse_expression(0)?;

        Ok((
            ASTNode::Lambda {
                parameters: parameters.clone(),
                body: Box::new(body.flatten_locations()),
                return_type,
            }
            .located((start_pos, end_post)),
            (start_pos, end_post)
        ))
    }

    fn parse_block_expression(&mut self) -> Result<ASTNode> {
        self.position += 1; // Skip the '{' character
        let mut expressions = Vec::new();
        let start_pos = self.position;

        while !self.peek_token("}") {
            self.skip_whitespaces();

            if self.peek_token("}") {
                break;
            }

            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let (stmt, _) = self.parse_statement()?;
            expressions.push(stmt.clone());
            self.skip_whitespaces();
            if self.peek_token(";") {
                self.position += 1; // Skip the semicolon

                self.skip_whitespaces();

                if self.peek_token("}") {
                    // If we encounter a '}' after a semicolon, we can stop parsing
                    return Err(RealityError::UnexpectedEndOfBlock);
                }
            } else if !self.peek_token("}") {
                return Err(RealityError::ExpectedToken("statement".to_string()));
            }
        }

        self.position += 1; // Skip the '}' character
        let end_pos = self.position;

        Ok((
            build_block_from_statements(expressions.as_slice()).located((start_pos, end_pos)),
            (start_pos, end_pos),
        ))
    }

    fn parse_statement(&mut self) -> Result<ASTNode> {
        self.skip_whitespaces();
        if self.peek_token("let") {
            return self.parse_statement_let();
        }

        return self.parse_expression(0);
    }

    fn parse_statement_let(&mut self) -> Result<ASTNode> {
        self.position += 3; // Skip the "let" keyword
        let (annotation, pos) = self.parse_identifier()?;
        self.skip_whitespaces();
        self.consume_token("=")?;
        let (value, _) = self.parse_expression(0)?;
        self.skip_whitespaces();

        Ok((
            ASTNode::LetIn {
                variable: Annotation {
                    name: annotation.to_string(),
                    value: None,
                    location: pos,
                },
                value: Box::new(value),
                body: Box::new(unit()),
            }
            .located(pos),
            pos,
        ))
    }

    fn parse_callee(&mut self) -> Result<ASTNode> {
        if self.peek_token("(") {
            self.position += 1; // Skip the opening parenthesis
            let callee = self.parse_expression(0)?;
            if self.peek_token(")") {
                self.position += 1; // Skip the closing parenthesis
                return Ok(callee);
            }
        } else if let Ok((identifier, pos)) = self.parse_identifier() {
            return Ok((
                ASTNode::Identifier(Annotation {
                    name: identifier.to_string(),
                    value: None,
                    location: pos,
                })
                .located(pos),
                pos,
            ));
        }

        Err(RealityError::ExpectedToken("callee".to_string()))
    }

    fn parse_call_expression(&mut self) -> Result<ASTNode> {
        let start_pos = self.position;
        let (callee, callee_pos) = self.parse_callee()?;

        if !self.peek_token("(") {
            return Ok((callee.located(callee_pos), callee_pos));
        }

        self.position += 1; // Skip the opening parenthesis

        let mut arguments = Vec::new();

        while !self.peek_token(")") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let (argument, _) = self.parse_expression(0)?;
            arguments.push(argument);
            self.skip_whitespaces();
            if self.peek_token(",") {
                self.position += 1; // Skip the comma
            } else if !self.peek_token(")") {
                return Err(RealityError::ExpectedToken(")".to_string()));
            }
        }

        self.position += 1; // Skip the closing parenthesis
        let end_pos = self.position;

        let pos = (start_pos, end_pos);

        Ok((
            ASTNode::Application {
                function: Box::new(callee),
                arguments,
            }
            .located(pos),
            pos,
        ))
    }
    
    fn parse_if_expression(&mut self) -> Result<ASTNode> {
        let (_, (start_pos, _)) = self.consume_token("if")?;

        let (condition, _) = self.parse_expression(0)?;

        let (then_branch, _) = self.parse_block_expression()?;

        self.consume_token("else")?;

        let (else_branch, (_, end_pos)) = self.parse_block_expression()?;

        let position = (start_pos, end_pos);

        Ok((
            ASTNode::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: Box::new(else_branch),
            }
            .located(position),
            position,
        ))
    }

    fn parse_operator(&mut self) -> Result<&'a str> {
        self.skip_whitespaces();
        let start = self.position;
        for (op, _) in &self.operators {
            if self.input[self.position..].starts_with(op) {
                self.position += op.len();
                return Ok((op, (start, self.position)));
            }
        }
        Err(RealityError::ExpectedToken("operator".to_string()))
    }

    fn parse_expression(&mut self, min_precedence: usize) -> Result<ASTNode> {
        self.skip_whitespaces();
        let (mut lhs, (start_pos, _)) = self.parse_term()?; // a number, identifier, parenthesis, etc.

        loop {
            self.skip_whitespaces();

            // Try to parse an operator
            let saved_pos = self.position;
            if let Ok((op, _)) = self.parse_operator() {
                let operators = self.operators.clone();
                if let Some((assoc, prec, OperatorType::Infix(f))) = operators.get(op) {
                    if *prec < min_precedence {
                        self.position = saved_pos;
                        break;
                    }

                    // For left-associative operators, precedence stays the same.
                    // For right-associative, we increase precedence for the recursive call.
                    let next_min_prec = match assoc {
                        Associativity::Left => *prec + 1,
                        Associativity::Right => *prec,
                        Associativity::NonAssociative => *prec + 1,
                    };

                    // After parsing the operator, parse the RHS
                    let (rhs, _) = self.parse_expression(next_min_prec)?;

                    lhs = f(&lhs, &rhs)?;
                    continue;
                } else {
                    // If the operator is not recognized, reset the position
                    self.position = saved_pos;

                    return Err(RealityError::UnexpectedOperator(op.to_string()));
                }
            }

            break;
        }

        let end_pos = self.position;

        Ok((lhs, (start_pos, end_pos)))
    }

    fn parse_parameters(&mut self, end: &'a str) -> Result<Vec<Annotation<Option<Type>>>> {
        let start = self.position;
        let mut parameters = Vec::new();

        while !self.peek_token(end) {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let start_id = self.position;
            let (param, _) = self.parse_identifier()?;

            if self.peek_token(":") {
                self.position += 1; // Skip the ':'
                let (type_annotation, (_, end)) = self.parse_type()?;
                parameters.push(Annotation {
                    name: param.to_string(),
                    value: Some(type_annotation),
                    location: (start_id, end),
                });
            } else {
                parameters.push(Annotation {
                    name: param.to_string(),
                    value: None,
                    location: (start_id, start_id),
                });
            }

            self.skip_whitespaces();
            if self.peek_token(",") {
                self.position += 1; // Skip the comma
            } else if !self.peek_token(end) {
                return Err(RealityError::ExpectedToken(end.to_string()));
            }
        }

        self.position += 1;

        Ok((parameters, (start - 1, self.position)))
    }

    fn parse_literal_float_prefixed(&mut self) -> Result<Literal> {
        self.position += 1;

        let (integer_part, pos) = self.parse_literal_integer()?;

        let fmt_float = format!("0.{}", integer_part);

        return match fmt_float.parse::<f64>() {
            Ok(value) => Ok((Literal::Float(value), pos)),
            Err(_) => Err(RealityError::ExpectedToken("float literal".to_string())),
        };
    }

    fn parse_literal_integer(&mut self) -> Result<i64> {
        let start = self.position;
        while self.position < self.input.len()
            && self.input[self.position..]
                .chars()
                .next()
                .unwrap()
                .is_numeric()
        {
            self.position += 1;
        }
        if start == self.position {
            return Err(RealityError::ExpectedToken("integer literal".to_string()));
        }
        let literal = &self.input[start..self.position];
        match literal.parse::<i64>() {
            Ok(value) => Ok((value, (start, self.position))),
            Err(_) => Err(RealityError::ExpectedToken("integer literal".to_string())),
        }
    }

    fn parse_literal_string(&mut self, character: char) -> Result<String> {
        if self.input[self.position..].starts_with(character) {
            self.position += 1; // Skip the opening quote
            let start = self.position;
            while self.position < self.input.len()
                && !self.input[self.position..].starts_with(character)
            {
                self.position += 1;
            }
            if self.position < self.input.len() {
                let literal = &self.input[start..self.position];
                self.position += 1; // Skip the closing quote
                return Ok((literal.to_string(), (start, self.position)));
            }
        }
        Err(RealityError::ExpectedToken("string literal".to_string()))
    }

    fn parse_literal_boolean(&mut self) -> Result<bool> {
        if let Ok(("true", pos)) = self.expect_token("true") {
            return Ok((true, pos));
        } else if let Ok(("false", pos)) = self.expect_token("false") {
            return Ok((false, pos));
        }

        Err(RealityError::ExpectedToken("boolean literal".to_string()))
    }

    fn parse_literal_number(&mut self) -> Result<Literal> {
        let (integer_part, pos) = self.parse_literal_integer()?;
        if self.peek_token(".") {
            self.position += 1; // Skip the dot
            let (fractional_part, _) = if let Ok((i, _)) = self.parse_literal_integer() {
                (i, pos)
            } else {
                (0, pos)
            };
            let fmt_value = format!("{}.{}", integer_part, fractional_part);
            let value = match fmt_value.parse::<f64>() {
                Ok(v) => v,
                Err(_) => return Err(RealityError::ExpectedToken("float literal".to_string())),
            };
            return Ok((Literal::Float(value), (pos.0, self.position)));
        }

        Ok((Literal::Integer(integer_part), pos))
    }

    fn parse_identifier(&mut self) -> Result<&'a str> {
        self.skip_whitespaces();
        let start = self.position;
        while self.position < self.input.len()
            && self.input[self.position..]
                .chars()
                .next()
                .unwrap()
                .is_alphanumeric()
        {
            self.position += 1;
        }
        if start == self.position {
            return Err(RealityError::ExpectedToken("identifier".to_string()));
        }

        let identifier = &self.input[start..self.position];

        if self.is_reserved(identifier) {
            self.position = start;
            return Err(RealityError::ReservedKeyword(identifier.to_string()));
        }

        let end_pos = self.position;

        self.skip_whitespaces();
        
        Ok((identifier, (start, end_pos)))
    }
}


fn operator(name: &str, a: &ASTNode, b: &ASTNode) -> ASTNode {
    ASTNode::Application {
        function: Box::new(ASTNode::Identifier(Annotation {
            name: name.to_string(),
            value: None,
            location: (0, 0), // Placeholder for location, can be adjusted later
        })),
        arguments: vec![a.clone(), b.clone()],
    }
}

pub fn add_default_operators(parser: &mut Parser) {
    parser.add_operator(
        "+",
        Associativity::Left,
        10,
        OperatorType::Infix(|a, b| Ok(operator("+", a, b))),
    );
    parser.add_operator(
        "-",
        Associativity::Left,
        10,
        OperatorType::Infix(|a, b| Ok(operator("-", a, b))),
    );
    parser.add_operator(
        "*",
        Associativity::Left,
        20,
        OperatorType::Infix(|a, b| Ok(operator("*", a, b))),
    );
    parser.add_operator(
        "/",
        Associativity::Left,
        20,
        OperatorType::Infix(|a, b| Ok(operator("/", a, b))),
    );

    parser.add_operator(
        "==",
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(|a, b| Ok(operator("==", a, b))),
    );

    parser.add_operator(
        "!=",
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(|a, b| Ok(operator("!=", a, b))),
    );

    parser.add_operator(
        "<",
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(|a, b| Ok(operator("<", a, b))),
    );

    parser.add_operator(
        ">",
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(|a, b| Ok(operator(">", a, b))),
    );

    parser.add_operator(
        "<=",
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(|a, b| Ok(operator("<=", a, b))),
    );

    parser.add_operator(
        ">=",
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(|a, b| Ok(operator(">=", a, b))),
    );

    parser.add_operator(
        "&&",
        Associativity::Left,
        40,
        OperatorType::Infix(|a, b| Ok(operator("&&", a, b))),
    );

    parser.add_operator(
        "||",
        Associativity::Left,
        40,
        OperatorType::Infix(|a, b| Ok(operator("||", a, b))),
    );

    parser.add_operator(
        "!",
        Associativity::Left,
        50,
        OperatorType::Prefix(|a| {
            Ok(ASTNode::Application {
                function: Box::new(ASTNode::Identifier(Annotation {
                    name: "!".to_string(),
                    value: None,
                    location: (0, 0), // Placeholder for location, can be adjusted later
                })),
                arguments: vec![a.clone()],
            })
        }),
    );
}
