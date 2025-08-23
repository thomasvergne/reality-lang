use std::collections::HashMap;

use reality_ast::{
    ASTNode, ToplevelNode, build_block_from_statements,
    internal::{annotation::Annotation, literal::Literal, types::Type},
    unit,
};
use reality_error::RealityError;

#[derive(Debug, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
    NonAssociative,
}

type Position = (usize, usize);
type Result<T> = std::result::Result<(T, Position), RealityError>;

type BinaryFunction = fn(ASTNode, ASTNode) -> ASTNode;
type UnaryFunction = fn(ASTNode) -> ASTNode;

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum OperatorType {
    Infix(fn(&mut Parser) -> Result<()>, BinaryFunction),
    Prefix(fn(&mut Parser) -> Result<()>, UnaryFunction),
    Postfix(fn(&mut Parser) -> Result<()>, UnaryFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    pub input: String,
    pub position: usize,
    pub file: String,
    operators: Vec<(Associativity, usize, OperatorType)>,
}

impl Parser {
    pub fn new<'a>(input: &'a str, file: &'a str) -> Self {
        Parser {
            input: input.to_string(),
            position: 0,
            file: file.to_string(),
            operators: Vec::new(),
        }
    }

    pub fn add_operator(
        &mut self,
        associativity: Associativity,
        precedence: usize,
        operator_type: OperatorType,
    ) {
        self.operators
            .push((associativity, precedence, operator_type));
    }

    fn skip_whitespaces(&mut self) {
        while self.position < self.input.len()
            && self.input[self.position..].starts_with(char::is_whitespace)
        {
            self.position += 1;
        }
    }

    fn expect_token<'a>(&mut self, token: &'a str) -> Result<&'a str> {
        self.skip_whitespaces();
        let start = self.position;
        if self.input[self.position..].starts_with(token) {
            self.position += token.len();
            Ok((token, (start, self.position)))
        } else {
            Err(RealityError::ExpectedToken(token.to_string()))
        }
    }

    fn is_reserved(&self, input: String) -> bool {
        match input.as_str() {
            "let" | "in" | "if" | "else" | "fn" | "pub" | "import" | "type" => true,
            _ => false,
        }
    }

    fn consume_token<'a>(&mut self, token: &'a str) -> Result<&'a str> {
        self.skip_whitespaces();
        let start = self.position;
        if self.peek_token(token) {
            self.position += token.len();
            let end = self.position;
            self.skip_whitespaces();
            Ok((token, (start, end)))
        } else {
            Err(RealityError::ExpectedToken(token.to_string()))
        }
    }

    fn peek_token<'a>(&self, token: &'a str) -> bool {
        self.input[self.position..].starts_with(token)
    }

    pub fn parse_program(&mut self) -> std::result::Result<Vec<ToplevelNode>, RealityError> {
        let mut nodes = Vec::new();

        while self.position < self.input.len() {
            let (node, span) = self.parse_toplevel()?;

            if self.peek_token(";") {
                self.consume_token(";")?;
            }

            self.skip_whitespaces();

            nodes.push(node.located(span, self.file.clone()));
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
        }

        if self.peek_token("fn") {
            return self.parse_top_function();
        }

        if self.peek_token("import") {
            return self.parse_top_require();
        }

        if self.peek_token("type") {
            return self.parse_top_type_alias();
        }

        if self.peek_token("pub") {
            return self.parse_top_public();
        }

        if self.peek_token("mod") {
            return self.parse_top_module();
        }

        if self.peek_token("struct") {
            return self.parse_top_struct();
        }

        if self.peek_token("extern") {
            return self.parse_top_external();
        }

        if self.peek_token("property") {
            return self.parse_top_property();
        }

        if self.peek_token("impl") {
            return self.parse_top_impl();
        }

        Err(RealityError::ExpectedToken("<toplevel>".to_string()))
    }

    fn parse_top_impl(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("impl")?;

        self.consume_token("fn")?;

        self.consume_token("(")?;

        let (id, _) = self.parse_identifier()?;

        self.consume_token(":")?;

        let (type_, _) = self.parse_type()?;

        self.consume_token(")")?;

        let (impl_name, _) = self.parse_identifier()?;

        let mut generics = vec![];

        if self.peek_token("[") {
            self.consume_token("[")?;

            while !self.peek_token("]") {
                let (generic, _) = self.parse_identifier()?;
                generics.push(generic.to_string());

                if self.peek_token(",") {
                    self.consume_token(",")?;
                } else if !self.peek_token("]") {
                    return Err(RealityError::ExpectedToken("]".to_string()));
                }
            }

            self.consume_token("]")?;
        }

        self.consume_token("(")?;

        let mut args = vec![];

        while !self.peek_token(")") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            let (arg_name, (start, _)) = self.parse_identifier()?;
            self.consume_token(":")?;
            let (arg_type, (_, en)) = self.parse_type()?;
            args.push(Annotation {
                name: arg_name,
                value: arg_type,
                location: (start, en),
            });
            self.skip_whitespaces();
            if self.peek_token(",") {
                self.consume_token(",")?;
            }
        }

        let (_, (_, end)) = self.consume_token(")")?;
        let mut end_pos = end;

        let mut return_type = Type::TypeIdentifier(vec!["unit".to_string()]);

        if self.peek_token("->") {
            self.consume_token("->")?;

            let (type_, (_, end)) = self.parse_type()?;
            return_type = type_;

            end_pos = end;
        }

        let (body, _) = self.parse_block_expression()?;

        Ok((ToplevelNode::Implementation {
            for_type: Annotation {
                name: id,
                value: type_,
                location: (start_pos, end_pos),
            },
            header: Annotation {
                name: impl_name,
                value: generics,
                location: (start_pos, end_pos),
            },
            arguments: args,
            return_type,
            body: Box::new(body),
        }, (start_pos, end_pos)))
    }

    fn parse_top_property(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("property")?;

        let (header, pos) = self.parse_identifier()?;

        let mut generics = vec![];

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
                    self.consume_token(",")?;
                }
            }
            self.consume_token("]")?;
        }

        self.consume_token("(")?;

        let mut end_pos;

        let mut args = Vec::new();

        while !self.peek_token(")") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }
            self.parse_identifier()?;
            self.consume_token(":")?;
            let (type_, _) = self.parse_type()?;
            args.push(type_);
            self.skip_whitespaces();
            if self.peek_token(",") {
                self.consume_token(",")?;
            }
        }

        let (_, (_, end)) = self.consume_token(")")?;
        end_pos = end;

        let mut return_type = Type::TypeIdentifier(vec!["unit".to_string()]);

        if self.peek_token("->") {
            self.consume_token("->")?;

            let (type_, (_, end)) = self.parse_type()?;
            return_type = type_;

            end_pos = end;
        }

        Ok((
            ToplevelNode::Property {
                header: Annotation {
                    name: header,
                    value: generics,
                    location: pos,
                },
                value: Type::TypeFunction {
                    parameters: args.iter().map(|p| p.clone()).collect::<Vec<_>>(),
                    return_type: Box::new(return_type),
                },
            },
            (start_pos, end_pos),
        ))
    }

    fn parse_type_application(&mut self) -> Result<Type> {
        self.skip_whitespaces();
        let start = self.position;

        let (identifier, _) = self.parse_scoped_identifier()?;

        self.skip_whitespaces();
        if self.peek_token("[") {
            self.consume_token("[")?;
            let mut arguments = Vec::new();

            while !self.peek_token("]") {
                self.skip_whitespaces();
                if self.position >= self.input.len() {
                    return Err(RealityError::UnexpectedEndOfFile);
                }
                let (arg, _) = self.parse_type()?;
                arguments.push(arg);
                self.skip_whitespaces();
                if self.peek_token(",") {
                    self.consume_token(",")?;
                } else if !self.peek_token("]") {
                    return Err(RealityError::ExpectedToken("]".to_string()));
                }
            }

            self.consume_token("]")?;
            self.skip_whitespaces();

            return Ok((
                Type::TypeApplication(Box::new(Type::TypeIdentifier(identifier)), arguments),
                (start, self.position),
            ));
        }

        Ok((Type::TypeIdentifier(identifier), (start, self.position)))
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
                    self.consume_token(",")?;
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
                self.consume_token(",")?;
            } else if !self.peek_token(")") {
                return Err(RealityError::ExpectedToken(")".to_string()));
            }
        }

        self.consume_token(")")?;

        let mut return_type = Type::TypeIdentifier(vec!["unit".to_string()]);

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

    fn parse_top_external(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("extern")?;

        self.consume_token("fn")?;

        let (name, (name_start, name_end)) = self.parse_identifier()?;

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
                self.consume_token(",")?;
            } else if !self.peek_token(")") {
                return Err(RealityError::ExpectedToken(")".to_string()));
            }
        }

        self.consume_token(")")?;

        let mut return_type = Type::TypeIdentifier(vec!["unit".to_string()]);

        if self.peek_token("->") {
            self.consume_token("->")?;
            let (ty, _) = self.parse_type()?;

            return_type = ty;
        }

        Ok((
            ToplevelNode::ExternalFunction {
                name: Annotation {
                    name: name.to_string(),
                    value: Vec::new(),
                    location: (name_start, name_end),
                },
                parameters,
                return_type,
            },
            (start_pos, self.position),
        ))
    }

    fn parse_top_module(&mut self) -> Result<ToplevelNode> {
        let (_, (start, _)) = self.consume_token("mod")?;

        let (name, _) = self.parse_identifier()?;

        let mut nodes = Vec::new();

        self.consume_token("{")?;

        while !self.peek_token("}") {
            let (node, _) = self.parse_toplevel()?;
            nodes.push(node);

            self.skip_whitespaces();

            if self.peek_token(";") {
                self.consume_token(";")?;
            }
        }

        let (_, (_, end)) = self.consume_token("}")?;

        Ok((
            ToplevelNode::ModuleDeclaration {
                name: name.to_string(),
                body: nodes,
            },
            (start, end),
        ))
    }

    fn parse_top_public(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("pub")?;

        let (node, (_, end_pos)) = self.parse_toplevel()?;

        Ok((
            ToplevelNode::PublicDeclaration(Box::new(node)),
            (start_pos, end_pos),
        ))
    }

    fn parse_top_struct(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("struct")?;

        let (name, (name_start, name_end)) = self.parse_identifier()?;
        let mut annot_end = name_end;

        let mut parameters = Vec::new();

        if self.peek_token("[") {
            self.consume_token("[")?;
            while !self.peek_token("]") {
                self.skip_whitespaces();
                if self.position >= self.input.len() {
                    return Err(RealityError::UnexpectedEndOfFile);
                }
                let (generic, _) = self.parse_identifier()?;
                parameters.push(generic.to_string());
                self.skip_whitespaces();
                if self.peek_token(",") {
                    self.consume_token(",")?;
                } else if !self.peek_token("]") {
                    return Err(RealityError::ExpectedToken("]".to_string()));
                }
            }
            let (_, (_, end_pos_2)) = self.consume_token("]")?;

            annot_end = end_pos_2;
        }

        self.consume_token("{")?;

        let mut fields = HashMap::new();

        while !self.peek_token("}") {
            let (field, _) = self.parse_identifier()?;
            self.consume_token(":")?;
            let (field_type, _) = self.parse_type()?;

            fields.insert(field, field_type);

            self.skip_whitespaces();

            if self.peek_token(",") {
                self.consume_token(",")?;
            } else if !self.peek_token("}") {
                return Err(RealityError::ExpectedToken("}".to_string()));
            }
        }

        self.consume_token("}")?;

        Ok((
            ToplevelNode::StructureDeclaration {
                header: Annotation {
                    name: name.to_string(),
                    value: parameters,
                    location: (name_start, annot_end),
                },
                fields,
            },
            (start_pos, self.position),
        ))
    }

    fn parse_top_require(&mut self) -> Result<ToplevelNode> {
        let (_, (start_pos, _)) = self.consume_token("import")?;

        let (module, (_, end_pos)) = self.parse_scoped_identifier()?;

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
                    self.consume_token(",")?;
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
        let (_, (start, _)) = self.consume_token("const")?;

        let (name, _) = self.parse_identifier()?;

        let mut annotation = None;

        if self.peek_token(":") {
            self.consume_token(":")?;
            let (type_annotation, _) = self.parse_type()?;
            annotation = Some(type_annotation);
        }

        self.consume_token("=")?;

        let (value, (_, end)) = self.parse_expression(0)?;

        Ok((
            ToplevelNode::ConstantDeclaration {
                variable: Annotation {
                    name: name.to_string(),
                    value: annotation,
                    location: (start, end),
                },
                value: Box::new(value),
            },
            (start, end),
        ))
    }

    fn parse_type_pointer(&mut self) -> Result<Type> {
        let (_, (start, _)) = self.consume_token("*")?;
        let (pointed_type, (_, end)) = self.parse_type()?;
        Ok((
            Type::TypeApplication(
                Box::new(Type::TypeIdentifier(vec!["pointer".to_string()])),
                vec![pointed_type],
            ),
            (start, end),
        ))
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
                self.consume_token(",")?;
            }
        }
        self.consume_token(")")?;

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
                ASTNode::Literal(Literal::String(string_literal)).located(pos, self.file.clone()),
                pos,
            ));
        } else if self.peek_token("\"") {
            let (string_literal, pos) = self.parse_literal_string('"')?;
            return Ok((
                ASTNode::Literal(Literal::String(string_literal)).located(pos, self.file.clone()),
                pos,
            ));
        } else if self.peek_token("true") || self.peek_token("false") {
            let (literal, pos) = self.parse_literal_boolean()?;
            return Ok((
                ASTNode::Literal(Literal::Boolean(literal)).located(pos, self.file.clone()),
                pos,
            ));
        } else if self.input[self.position..]
            .chars()
            .next()
            .unwrap()
            .is_numeric()
        {
            let (literal, pos) = self.parse_literal_number()?;
            return Ok((
                ASTNode::Literal(literal).located(pos, self.file.clone()),
                pos,
            ));
        } else if self.peek_token("if") {
            return self.parse_if_expression();
        } else if self.peek_token("let") {
            return self.parse_let_declaration();
        } else if self.peek_token("{") {
            return self.parse_block_expression();
        } else if self.peek_token(".") {
            return self
                .parse_literal_float_prefixed()
                .map(|(lit, pos)| (ASTNode::Literal(lit).located(pos, self.file.clone()), pos));
        } else if self.peek_token("|") {
            return self.parse_lambda();
        } else if let Ok((ident, pos)) = self.parse_scoped_identifier() {
            if self.peek_token("{") && let Some(id) = ident.get(0) {
                return self.parse_structure_creation(id.to_string(), pos.0);
            }

            return Ok((
                ASTNode::Identifier(Annotation {
                    name: ident,
                    value: None,
                    location: pos,
                })
                .located(pos, self.file.clone()),
                pos,
            ));
        } else if self.peek_token("(") {
            let (_, (start, _)) = self.consume_token("(")?;
            let (expr, _) = self.parse_expression(0)?;
            let (_, (_, end)) = self.consume_token(")")?;
            return Ok((expr, (start, end)));
        }

        Err(RealityError::ExpectedToken("term".to_string()))
    }

    fn parse_structure_creation(&mut self, id: String, start_pos: usize) -> Result<ASTNode> {
        self.consume_token("{")?;

        let mut fields = HashMap::new();

        while !self.peek_token("}") {
            self.skip_whitespaces();
            if self.position >= self.input.len() {
                return Err(RealityError::UnexpectedEndOfFile);
            }

            let (field, _) = self.parse_identifier()?;
            self.consume_token(":")?;
            let (field_type, _) = self.parse_expression(0)?;

            fields.insert(field, field_type);

            self.skip_whitespaces();
            if self.peek_token(",") {
                self.consume_token(",")?;
            }
        }
        let (_, (_, end_pos)) = self.consume_token("}")?;

        return Ok((
            ASTNode::StructureCreation {
                structure_name: Annotation {
                    name: id,
                    value: None,
                    location: (start_pos, end_pos),
                },
                fields,
            }
            .located((start_pos, end_pos), self.file.clone()),
            (start_pos, end_pos),
        ));
    }

    fn parse_let_declaration(&mut self) -> Result<ASTNode> {
        self.consume_token("let")?;

        let (name, pos) = self.parse_identifier()?;

        let mut ty = None;

        if self.peek_token(":") {
            self.consume_token(":")?;

            let (parsed_ty, _) = self.parse_type()?;
            ty = Some(parsed_ty);
        }

        self.consume_token("=")?;
        let (value, _) = self.parse_expression(0)?;

        Ok((
            ASTNode::LetIn {
                variable: Annotation {
                    name: name.to_string(),
                    value: ty,
                    location: pos,
                },
                value: Box::new(value),
                body: Box::new(unit()),
                return_ty: None,
            }
            .located(pos, self.file.clone()),
            pos,
        ))
    }

    fn parse_lambda(&mut self) -> Result<ASTNode> {
        let (_, (start_pos, _)) = self.consume_token("|")?;
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
            .located((start_pos, end_post), self.file.clone()),
            (start_pos, end_post),
        ))
    }

    fn parse_block_expression(&mut self) -> Result<ASTNode> {
        self.consume_token("{")?;
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
                self.consume_token(";")?;

                self.skip_whitespaces();

                if self.peek_token("}") {
                    // If we encounter a '}' after a semicolon, we can stop parsing
                    return Err(RealityError::UnexpectedEndOfBlock);
                }
            } else if !self.peek_token("}") {
                return Err(RealityError::ExpectedToken("statement".to_string()));
            }
        }

        self.consume_token("}")?;
        let end_pos = self.position;

        Ok((
            build_block_from_statements(expressions.as_slice())
                .located((start_pos, end_pos), self.file.clone()),
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
        self.consume_token("let")?;
        let (annotation, pos) = self.parse_identifier()?;

        let mut ty = None;

        if self.peek_token(":") {
            self.consume_token(":")?;

            let (parsed_ty, _) = self.parse_type()?;
            ty = Some(parsed_ty);
        }

        self.consume_token("=")?;
        let (value, _) = self.parse_expression(0)?;

        Ok((
            ASTNode::LetIn {
                variable: Annotation {
                    name: annotation.to_string(),
                    value: ty,
                    location: pos,
                },
                value: Box::new(value),
                body: Box::new(unit()),
                return_ty: None,
            }
            .located(pos, self.file.clone()),
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
                return_ty: None,
            }
            .located(position, self.file.clone()),
            position,
        ))
    }

    fn parse_operator(
        &mut self,
    ) -> Result<(
        std::result::Result<BinaryFunction, UnaryFunction>,
        usize,
        Associativity,
    )> {
        self.skip_whitespaces();
        let start = self.position;
        let mut err: Option<RealityError> = None;

        // Clone the operators list to avoid multiple mutable borrows of self
        let mut sorted_operators = self.operators.clone();
        sorted_operators.sort_by(|a, b| a.1.cmp(&b.1));

        let mut operators_iter = sorted_operators.into_iter();

        while let Some((assoc, prec, op)) = operators_iter.next() {
            let op_f = get_function(op);
            let result = op_f(self);

            match result {
                Ok(((), (start_tmp, end_tmp))) => match op {
                    OperatorType::Infix(_, f) => {
                        let f_ret = Ok(f.clone());

                        return Ok(((f_ret, prec, assoc.clone()), (start, start)));
                    }
                    OperatorType::Prefix(_, f) => {
                        return Ok((
                            (Err(f.clone()), prec, assoc.clone()),
                            (start_tmp, start_tmp),
                        ));
                    }

                    OperatorType::Postfix(_, f) => {
                        return Ok(((Err(f.clone()), prec, assoc.clone()), (start, end_tmp)));
                    }
                },

                Err(err_) => {
                    self.position = start;
                    err = Some(err_);

                    continue;
                }
            }
        }

        Err(err.unwrap_or(RealityError::ExpectedToken("operator".to_string())))
    }

    fn parse_expression(&mut self, min_precedence: usize) -> Result<ASTNode> {
        self.skip_whitespaces();

        if let Ok(((f, prec, _), (start, _))) = self.parse_operator() {
            let (rhs, (_, end_pos)) = self.parse_expression(prec + 1)?;

            match f {
                Err(f) => {
                    return Ok((
                        f(rhs).located((start, end_pos), self.file.clone()),
                        (start, end_pos),
                    ));
                }

                Ok(_) => {
                    return Err(RealityError::ExpectedToken(
                        "left-hand side of binary operator".to_string(),
                    ));
                }
            }
        }

        let (mut lhs, (start_pos, end_pos)) = self.parse_term()?; // a number, identifier, parenthesis, etc.
        let mut end = end_pos;

        loop {
            self.skip_whitespaces();

            // Try to parse an operator
            let saved_pos = self.position;

            if let Ok(((f, prec, assoc), (_, end_postfix_pos))) = self.parse_operator() {
                if prec < min_precedence {
                    self.position = saved_pos;
                    break;
                }

                // For left-associative operators, precedence stays the same.
                // For right-associative, we increase precedence for the recursive call.
                let next_min_prec = match assoc {
                    Associativity::Left => prec + 1,
                    Associativity::Right => prec,
                    Associativity::NonAssociative => prec + 1,
                };

                match f {
                    Ok(f) => {
                        // After parsing the operator, parse the RHS
                        let (rhs, (_, end_pos)) = self.parse_expression(next_min_prec)?;

                        lhs = f(lhs, rhs).located((start_pos, end_pos), self.file.clone());
                        end = end_pos;
                        continue;
                    }

                    Err(f) => {
                        lhs = f(lhs).located((start_pos, end_postfix_pos), self.file.clone());
                        end = end_postfix_pos;
                    }
                }
            } else if let Ok(_) = self.consume_token("(") {
                let mut arguments = Vec::new();

                while !self.peek_token(")") {
                    self.skip_whitespaces();
                    let (arg, _) = self.parse_expression(0)?;
                    arguments.push(arg);
                    self.skip_whitespaces();
                    if self.peek_token(",") {
                        self.consume_token(",")?;
                    } else if !self.peek_token(")") {
                        return Err(RealityError::ExpectedToken(")".to_string()));
                    }
                }

                self.consume_token(")")?;

                lhs = ASTNode::Application {
                    function: Box::new(lhs),
                    arguments,
                    function_type: None,
                }
                .located((start_pos, end_pos), self.file.clone());

                continue;
            } else if let Ok(_) = self.consume_token(".") {
                // Field access
                let (field, (_, end_field_pos)) = self.parse_identifier()?;

                lhs = ASTNode::StructureAccess {
                    structure: Box::new(lhs),
                    field: field.to_string(),
                }
                .located((start_pos, end_field_pos), self.file.clone());

                end = end_field_pos;
                continue;
            }

            break;
        }

        Ok((lhs, (start_pos, end)))
    }

    fn parse_parameters<'a>(&mut self, end: &'a str) -> Result<Vec<Annotation<Option<Type>>>> {
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
                self.consume_token(":")?;
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
                self.consume_token(",")?;
            } else if !self.peek_token(end) {
                return Err(RealityError::ExpectedToken(end.to_string()));
            }
        }

        self.consume_token(end)?;

        Ok((parameters, (start - 1, self.position)))
    }

    fn parse_literal_float_prefixed(&mut self) -> Result<Literal> {
        let (_, (start_pos, _)) = self.consume_token(".")?;

        let (integer_part, (_, end_pos)) = self.parse_literal_integer()?;

        let fmt_float = format!("0.{}", integer_part);

        return match fmt_float.parse::<f64>() {
            Ok(value) => Ok((Literal::Float(value), (start_pos, end_pos))),
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
            self.consume_token(".")?;
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

    fn parse_scoped_identifier(&mut self) -> Result<Vec<String>> {
        let mut identifiers = Vec::new();
        let (start_id, (start, end)) = self.parse_identifier()?;
        identifiers.push(start_id.to_string());

        let mut end_pos = end;

        while self.peek_token("::") {
            self.consume_token("::")?;

            if self.peek_token("*") {
                let (_, (_, end)) = self.consume_token("*")?;
                identifiers.push("*".to_string());
                end_pos = end;
                continue;
            }

            let (ident, (_, end)) = self.parse_identifier()?;
            identifiers.push(ident.to_string());
            end_pos = end;
        }

        Ok((identifiers, (start, end_pos)))
    }

    fn parse_identifier(&mut self) -> Result<String> {
        self.skip_whitespaces();
        let start = self.position;
        while self.position < self.input.len()
            && (self.input[self.position..]
                .chars()
                .next()
                .unwrap()
                .is_alphanumeric()
                || self.input[self.position..].starts_with('_'))
        {
            self.position += 1;
        }
        if start == self.position {
            return Err(RealityError::ExpectedToken("identifier".to_string()));
        }

        let identifier = self.input[start..self.position].to_string();

        if self.is_reserved(identifier.clone()) {
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
            name: vec![name.to_string()],
            value: None,
            location: (0, 0), // Placeholder for location, can be adjusted later
        })),
        arguments: vec![a.clone(), b.clone()],
        function_type: None,
    }
}

pub fn add_default_operators(parser: &mut Parser) {
    parser.add_operator(
        Associativity::Left,
        10,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("+")?;
                Ok(((), pos))
            },
            |a, b| operator("add", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::Left,
        10,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("-")?;
                Ok(((), pos))
            },
            |a, b| operator("sub", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::Left,
        20,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("*")?;
                Ok(((), pos))
            },
            |a, b| operator("mul", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::Left,
        20,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("/")?;
                Ok(((), pos))
            },
            |a, b| operator("div", &a, &b),
        ),
    );

    parser.add_operator(
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("==")?;
                Ok(((), pos))
            },
            |a, b| operator("equals", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("!=")?;
                Ok(((), pos))
            },
            |a, b| operator("not_equals", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("<")?;
                Ok(((), pos))
            },
            |a, b| operator("less_than", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token(">")?;
                Ok(((), pos))
            },
            |a, b| operator("greater_than", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("<=")?;
                Ok(((), pos))
            },
            |a, b| operator("less_than_or_equal", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::NonAssociative,
        30,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token(">=")?;
                Ok(((), pos))
            },
            |a, b| operator("greater_than_or_equal", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::Left,
        40,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("&&")?;
                Ok(((), pos))
            },
            |a, b| operator("and", &a, &b),
        ),
    );
    parser.add_operator(
        Associativity::Left,
        40,
        OperatorType::Infix(
            |p| {
                let (_, pos) = p.consume_token("||")?;
                Ok(((), pos))
            },
            |a, b| operator("or", &a, &b),
        ),
    );

    parser.add_operator(
        Associativity::Left,
        50,
        OperatorType::Prefix(
            |p| {
                let (_, pos) = p.consume_token("!")?;
                Ok(((), pos))
            },
            |a| {
                ASTNode::Application {
                    function: Box::new(ASTNode::Identifier(Annotation {
                        name: vec!["not".to_string()],
                        value: None,
                        location: (0, 0), // Placeholder for location, can be ajusté plus tard
                    })),
                    arguments: vec![a.clone()],
                    function_type: None,
                }
            },
        ),
    );
}

fn get_function(op: OperatorType) -> fn(&mut Parser) -> Result<()> {
    match op {
        OperatorType::Infix(f, _) => f,
        OperatorType::Prefix(f, _) => f,
        OperatorType::Postfix(f, _) => f,
    }
}
