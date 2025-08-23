use reality_ast::{internal::{annotation::Annotation, literal::Literal, types::{Type, TypeVariable}}, llir::{ToplevelLLIR, LLIR}, name_mangle};

pub struct CodeGeneration {}

impl CodeGeneration {
    pub fn new() -> Self {
        CodeGeneration {}
    }

    pub fn generate(&self, ast: Vec<ToplevelLLIR>) -> String {
        let mut result = String::new();

        for node in ast {
            result.push_str(&self.generate_node(node));
            result.push_str(";\n");
        }

        result
    }

    fn is_pointer_type(&self, ty: Type<String>) -> bool {
        match ty {
            Type::TypeIdentifier(name) if name == "pointer" => true,
            Type::TypeAliased(ty, _) => self.is_pointer_type(*ty),
            _ => false,
        }
    }

    fn generate_type(&self, ty: Type<String>, name: Option<String>) -> String {
        match ty {
            Type::TypeAliased(ty, alias) => {
                format!("/* {} */ {}", alias, self.generate_type(*ty, name))
            }

            Type::AnonymousStructure { fields } => {
                let mut result = String::new();
                result.push_str("struct {\n");
                for (name, ty) in fields {
                    result.push_str(&format!("  {};\n", self.generate_type(ty, Some(name))));
                }
                result.push_str(format!("}} {}", name.unwrap_or_default()).as_str());
                result
            }

            Type::TypeVariable(var) => {
                if let TypeVariable::Bound(ty) = &*var.borrow() {
                    self.generate_type(*ty.clone(), name)
                } else {
                    panic!("Unbound type variable")
                }
            }

            Type::TypeIdentifier(ty) => {
                let ty = match ty.as_str() {
                    "i8" => "int8_t".into(),
                    "i16" => "int16_t".into(),
                    "i32" => "int32_t".into(),
                    "i64" => "int64_t".into(),
                    "f32" => "float32_t".into(),
                    "f64" => "float64_t".into(),
                    "bool" => "bool".into(),
                    "u8" => "uint8_t".into(),
                    "u16" => "uint16_t".into(),
                    "u32" => "uint32_t".into(),
                    "u64" => "uint64_t".into(),
                    _ => ty.clone(),
                };

                format!("{} {}", name_mangle(ty), name.unwrap_or_default())
            }

            Type::TypeFunction { parameters, return_type } => {
                let mut function = String::new();

                let params = parameters
                    .into_iter()
                    .map(|ty| self.generate_type(ty, None))
                    .collect::<Vec<_>>();

                let return_type = self.generate_type(*return_type, None);

                function.push_str(&format!("{} (*{})(", return_type, name.unwrap_or_default()));

                for (i, param) in params.iter().enumerate() {
                    function.push_str(&format!(" {}", param));

                    if i < params.len() - 1 {
                        function.push_str(", ");
                    }
                }

                function.push(')');

                function
            }

            Type::TypeApplication(base, args) if self.is_pointer_type(*base.clone()) => {
                let mut result = String::new();
                result.push_str(&format!("{}* {}", self.generate_type(args[0].clone(), None), name.unwrap_or_default()));
                result
            }

            Type::TypeApplication(_, _) => {
                panic!("Type applications are not supported in code generation")
            }
        }
    }

    fn generate_node(&self, ast: ToplevelLLIR) -> String {
        match ast {
            ToplevelLLIR::ExternalFunction { name, parameters, return_type } => {
                // extern void printf(const char*);
                let mut result = String::new();
                result.push_str(&format!("extern {}(", self.generate_type(return_type, Some(name))));
                for (i, param) in parameters.iter().enumerate() {
                    result.push_str(&self.generate_type(param.value.clone(), Some(param.name.clone())));
                    if i < parameters.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(");");
                result
            }
            
            ToplevelLLIR::StructureDeclaration { header, fields } => {
                let mut result = String::new();
                result.push_str(&format!("typedef struct {} {{\n", name_mangle(header.clone())));

                for (field, ty) in fields {
                    result.push_str(&self.generate_type(ty, Some(field.clone())));

                    result.push_str(";");
                }
                
                result.push_str(&format!("}} {};", name_mangle(header)));
                result
            }

            ToplevelLLIR::FunctionDeclaration { name, parameters, return_type, body } => {
                let mut result = String::new();
                
                let name_with_type = self.generate_type(return_type, Some(name_mangle(name.clone())));

                result.push_str(&format!("{}(", name_with_type));

                for (i, Annotation { name, value, .. }) in parameters.iter().enumerate() {
                    result.push_str(&self.generate_type(value.clone(), Some(name_mangle(name.clone()))));

                    if i < parameters.len() - 1 {
                        result.push_str(", ");
                    }
                }

                result.push_str(") {");

                for (i, statement) in body.iter().enumerate() {
                    let statement_str = self.generate_expression(statement.clone());
                    if i == body.len() - 1 {
                        result.push_str(&format!("    return {};\n", statement_str));
                    } else {
                        result.push_str(&format!("    {};\n", statement_str));
                    }
                }

                result.push_str("\n}");

                result
            }

            ToplevelLLIR::ConstantDeclaration { name, annotation, value } => {
                let mut result = String::new();

                let name_with_type = self.generate_type(annotation, Some(name_mangle(name.clone())));

                result.push_str(&format!("{} = ", name_with_type));
                result.push_str(&self.generate_expression(*value));
                result.push_str(";");

                result
            }
        }
    }

    fn generate_expression(&self, expr: LLIR) -> String {
        match expr {
            LLIR::Update(target, value) => {
                format!("{} = {}", self.generate_expression(*target), self.generate_expression(*value))
            }

            LLIR::Reference(inner) => {
                format!("&{}", self.generate_expression(*inner))
            }
            LLIR::Dereference(inner) => {
                format!("*{}", self.generate_expression(*inner))
            }

            LLIR::Block(expressions) => {
                let mut result = String::new();
                result.push_str("{\n");
                for (_, expr) in expressions.iter().enumerate() {
                    let expr_str = self.generate_expression(expr.clone());
                    
                    result.push_str(&format!("    {};\n", expr_str));
                }
                result.push_str("}");
                result
            }
            
            LLIR::Literal(value) => match value {
                Literal::String(s) => format!("\"{}\"", s),
                Literal::Integer(n) => format!("{}", n),
                Literal::Boolean(b) => format!("{}", b),
                Literal::Float(f) => format!("{}", f),
                Literal::Character(c) => format!("'{}'", c),
            },
            LLIR::StructureCreation { structure_name, fields } => {
                let mut result = format!("(struct {}) {{\n", name_mangle(structure_name));
                for (field, value) in fields {
                    result.push_str(&format!("    .{} = {},\n", name_mangle(field), self.generate_expression(value)));
                }
                result.push_str("}");
                result
            }

            LLIR::StructureAccess { structure, field } => {
                format!("{}.{}", self.generate_expression(*structure), name_mangle(field))
            }

            LLIR::Let { name, value, annotation } => {
                let mut string = String::new();
                string.push_str(&self.generate_type(annotation, Some(name_mangle(name))));

                if let Some(value) = value {
                    string.push_str(&format!(" = {}", self.generate_expression(*value)));
                }

                string
            }

            LLIR::If { condition, then_branch, else_branch } => {
                format!("if ({}) {{\n{}\n}} else {{\n{}\n}}",
                    self.generate_expression(*condition),
                    self.generate_expression(*then_branch),
                    self.generate_expression(*else_branch)
                )
            }

            LLIR::Identifier(name) => {
                format!("{}", name_mangle(name))
            }

            LLIR::Application { function, arguments, .. } => {
                let mut result = format!("({})(", self.generate_expression(*function));
                for (i, arg) in arguments.iter().enumerate() {
                    result.push_str(&format!("{}", self.generate_expression(arg.clone())));

                    if i < arguments.len() - 1 {
                        result.push_str(", ");
                    }
                }

                result.push_str(")");

                result
            }
        }
    }
}
