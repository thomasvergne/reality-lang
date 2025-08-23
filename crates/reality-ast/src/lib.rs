// <==== AST MODULE ====>
//
// The basic blocks used to build programs in Reality. The set is explicitely
// designed to be minimal, and reusable. This may impact positively compiler
// programming experience.
//
// The AST is designed to be generic over types, allowing for type annotations
// and other metadata to be easily attached to nodes.
//
// The following variants represent the different types of AST nodes:
//
// (1). Literal stands for a basic building literal block.
// (2). Identifier stands for a named variable.
// (3). Application stands for function calls, such as f(x, y, ..., z)
// (4). Lambda stands for anonymous functions.
// (5). LetIn stands for variable bindings and scoping.
// (6). If stands for conditional expressions.
// (7). Located stands for source code locations.
//
// This module also defines the ToplevelNode enum, which represents the top-level
// constructs in a Reality program.
//
// Here is a non-exhaustive list of the top-level constructs:
//
// (1). ConstantDeclaration stands for constant variable declarations.
// (2). FunctionDeclaration stands for function declarations.
// (3). Require stands for module imports.
// (4). TypeAlias stands for type aliases.
//
// <==== AST MODULE ====>

use std::{collections::HashMap, fmt::Debug};

use crate::internal::{annotation::Annotation, literal::Literal, types::Type};

pub mod internal;
pub mod llir;

#[derive(Clone, PartialEq)]
pub enum ASTNode<N = Vec<String>, T = Option<Type<N>>> {
    Literal(Literal),
    Identifier(Annotation<T, N>),

    Application {
        function: Box<ASTNode<N, T>>,
        arguments: Vec<ASTNode<N, T>>,

        function_type: T,
    },

    Lambda {
        parameters: Vec<Annotation<T>>,
        return_type: T,
        body: Box<ASTNode<N, T>>,
    },

    LetIn {
        variable: Annotation<T>,
        value: Box<ASTNode<N, T>>,
        body: Box<ASTNode<N, T>>,
        return_ty: T,
    },

    If {
        condition: Box<ASTNode<N, T>>,
        then_branch: Box<ASTNode<N, T>>,
        else_branch: Box<ASTNode<N, T>>,

        return_ty: T,
    },

    Located {
        span: (usize, usize, String),
        node: Box<ASTNode<N, T>>,
    },

    StructureAccess {
        structure: Box<ASTNode<N, T>>,
        field: String,
    },

    StructureCreation {
        structure_name: Annotation<T>,
        fields: HashMap<String, ASTNode<N, T>>,
    }
}

#[derive(Clone, PartialEq)]
pub enum ToplevelNode<N = Vec<String>, T = Type<N>, AT = Option<Type<N>>> {
    ConstantDeclaration {
        variable: Annotation<AT>,
        value: Box<ASTNode<N, AT>>,
    },

    FunctionDeclaration {
        name: Annotation<Vec<String>>,
        parameters: Vec<Annotation<T>>,
        return_type: T,
        body: Box<ASTNode<N, AT>>,
    },

    ImportDeclaration(Vec<String>),

    TypeAlias {
        name: Annotation<Vec<String>>,
        body: T,
    },

    PublicDeclaration(Box<ToplevelNode<N, T, AT>>),

    ModuleDeclaration {
        name: String,
        body: Vec<ToplevelNode<N, T, AT>>,
    },

    Located {
        span: (usize, usize, String),
        node: Box<ToplevelNode<N, T, AT>>,
    },

    StructureDeclaration {
        header: Annotation<Vec<String>>,
        fields: HashMap<String, Type<N>>,
    }
}

pub type TypedASTNode<N = String, T = Type<N>> = ASTNode<N, T>;
pub type TypedToplevelNode<N = String, T = Type<N>, AT = Type<N>> = ToplevelNode<N, T, AT>;

impl<T: Debug, N: Debug> Debug for ASTNode<N, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTNode::StructureCreation { structure_name, fields } => {
                write!(f, "struct {:?} {{ ", structure_name)?;
                for (name, field) in fields {
                    write!(f, "{}: {:?}, ", name, field)?;
                }
                write!(f, "}}")
            }

            ASTNode::Literal(lit) => write!(f, "{:?}", lit),
            ASTNode::Identifier(id) => {
                write!(f, "{:?}", id.name)?;

                Ok(())
            }

            ASTNode::StructureAccess { structure, field } => {
                write!(f, "{:?}.{}", structure, field)
            }

            ASTNode::Application {
                function,
                arguments,
                ..
            } => {
                write!(f, "{:?}(", function)?;
                for (i, arg) in arguments.iter().enumerate() {
                    write!(f, "{:?}", arg)?;
                    if i < arguments.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            ASTNode::Lambda {
                parameters,
                return_type,
                body,
            } => {
                write!(f, "|")?;
                for (i, param) in parameters.iter().enumerate() {
                    write!(f, "{:?}", param)?;

                    if i < parameters.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "| -> {:?} {{ {:?} }}", return_type, body)
            }
            ASTNode::LetIn {
                variable,
                value,
                body,
                ..
            } => {
                write!(f, "let {:?} = {:?}; {:?}", variable, value, body)
            }
            ASTNode::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                write!(
                    f,
                    "if {:?} {{ {:?} }} else {{ {:?} }}",
                    condition, then_branch, else_branch
                )
            }
            ASTNode::Located { node, .. } => write!(f, "{:?}", node),
        }
    }
}

impl<T: Debug, AT: Debug, N: Debug> Debug for ToplevelNode<N, T, AT> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ToplevelNode::StructureDeclaration { header, fields } => {
                write!(f, "struct {} {{ ", header.name)?;
                for (i, (field_name, field_type)) in fields.iter().enumerate() {
                    write!(f, "{}: {:?}", field_name, field_type)?;
                    if i < fields.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }

            ToplevelNode::Located { node, .. } => write!(f, "{:?}", node),
            ToplevelNode::ModuleDeclaration { name, body } => {
                write!(f, "module {} {{ {:?} }}", name, body)
            }

            ToplevelNode::PublicDeclaration(inner) => {
                write!(f, "pub {:?}", inner)
            }

            ToplevelNode::ConstantDeclaration { variable, value } => {
                write!(f, "const {:?} = {:?};", variable, value)
            }
            ToplevelNode::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                write!(f, "fn {}<{:?}>(", name.name, name.value)?;
                for (i, param) in parameters.iter().enumerate() {
                    write!(f, "{:?}", param)?;
                    if i < parameters.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {:?} {{ {:?} }}", return_type, body)
            }
            ToplevelNode::ImportDeclaration(module) => {
                write!(f, "require \"{}\";", module.join("::").to_string())
            }
            ToplevelNode::TypeAlias { name, body } => write!(f, "type {:?} = {:?};", name, body),
        }
    }
}

impl<T, AT, N> ToplevelNode<N, T, AT> {
    pub fn located(self, span: (usize, usize), file: String) -> Self {
        ToplevelNode::Located {
            span: (span.0, span.1, file),
            node: Box::new(self),
        }
    }
}

impl<T: Clone, AT: Clone, N: Clone> ToplevelNode<N, T, AT> {
    pub fn flatten_locations(&self) -> ToplevelNode<N, T, AT> {
        match self {
            ToplevelNode::ModuleDeclaration { name, body } => ToplevelNode::ModuleDeclaration {
                name: name.clone(),
                body: body.iter().map(ToplevelNode::flatten_locations).collect(),
            },

            ToplevelNode::Located { node, .. } => node.flatten_locations(),

            ToplevelNode::ConstantDeclaration { variable, value } => {
                ToplevelNode::ConstantDeclaration {
                    variable: variable.clone(),
                    value: Box::new(value.flatten_locations()),
                }
            }

            ToplevelNode::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => ToplevelNode::FunctionDeclaration {
                name: name.clone(),
                parameters: parameters.clone(),
                return_type: return_type.clone(),
                body: Box::new(body.flatten_locations()),
            },

            ToplevelNode::PublicDeclaration(inner) => {
                ToplevelNode::PublicDeclaration(Box::new(inner.flatten_locations()))
            }

            ToplevelNode::TypeAlias { name, body } => ToplevelNode::TypeAlias {
                name: name.clone(),
                body: body.clone(),
            },

            ToplevelNode::ImportDeclaration(module) => {
                ToplevelNode::ImportDeclaration(module.clone())
            }

            ToplevelNode::StructureDeclaration { header, fields } => {
                ToplevelNode::StructureDeclaration {
                    header: header.clone(),
                    fields: fields.clone(),
                }
            }
        }
    }
}

impl<T: Clone> ASTNode<Vec<String>, T> {
    pub fn located(self, span: (usize, usize), file: String) -> Self {
        ASTNode::Located {
            span: (span.0, span.1, file),
            node: Box::new(self),
        }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, ASTNode::Identifier(Annotation { name, .. })
            if name.as_slice() == &["unit".to_string()]
        )
    }
}

impl<T: Clone, N: Clone> ASTNode<N, T> {
    pub fn flatten_locations(&self) -> ASTNode<N, T> {
        match self {
            ASTNode::StructureCreation { fields, structure_name } => {
                ASTNode::StructureCreation {
                    fields: fields
                        .iter()
                        .map(|(name, field)| (name.clone(), field.flatten_locations()))
                        .collect(),
                    structure_name: structure_name.clone(),
                }
            }

            ASTNode::StructureAccess { structure, field } => {
                ASTNode::StructureAccess {
                    structure: Box::new(structure.flatten_locations()),
                    field: field.clone(),
                }
            }

            ASTNode::Located { node, .. } => node.flatten_locations(),
            ASTNode::Application {
                function,
                arguments,
                function_type
            } => ASTNode::Application {
                function: Box::new(function.flatten_locations()),
                arguments: arguments.iter().map(ASTNode::flatten_locations).collect(),
                function_type: function_type.clone(),
            },
            ASTNode::LetIn {
                variable,
                value,
                body,
                return_ty,
            } => ASTNode::LetIn {
                variable: variable.clone(),
                value: Box::new(value.flatten_locations()),
                body: Box::new(body.flatten_locations()),
                return_ty: return_ty.clone(),
            },
            ASTNode::Lambda {
                parameters,
                body,
                return_type,
            } => ASTNode::Lambda {
                parameters: parameters.clone(),
                body: Box::new(body.flatten_locations()),
                return_type: return_type.clone(),
            },
            ASTNode::If {
                condition,
                then_branch,
                else_branch,
                return_ty
            } => ASTNode::If {
                condition: Box::new(condition.flatten_locations()),
                then_branch: Box::new(then_branch.flatten_locations()),
                else_branch: Box::new(else_branch.flatten_locations()),
                return_ty: return_ty.clone(),
            },
            ASTNode::Literal(literal) => ASTNode::Literal(literal.clone()),
            ASTNode::Identifier(ann) => ASTNode::Identifier(ann.clone()),
        }
    }
}

impl ASTNode<String, Type<String>> {
    pub fn free_variables(&self) -> HashMap<String, Type<String>> {
        match self {
            ASTNode::StructureCreation { fields, .. } => {
                fields.iter().flat_map(|(_, field)| field.free_variables()).collect()
            }

            ASTNode::StructureAccess { structure, .. } => structure.free_variables(),
            ASTNode::Located { node, .. } => node.free_variables(),
            ASTNode::Identifier(ann) => {
                let mut map = HashMap::new();
                map.insert(ann.name.clone(), ann.value.clone());
                map
            },
            ASTNode::Application { function, arguments, .. } => {
                let mut free = function.free_variables();
                for arg in arguments {
                    free.extend(arg.free_variables());
                }
                free
            }
            ASTNode::LetIn { variable, value, body, .. } => {
                let mut free = value.free_variables();
                free.extend(body.free_variables());
                free.retain(|v, _| v != &variable.name);
                free
            }
            ASTNode::Lambda { parameters, body, .. } => {
                let mut free = body.free_variables();
                
                for param in parameters {
                    free.retain(|v, _| v != &param.name);
                }

                free
            }
            ASTNode::If { condition, then_branch, else_branch, .. } => {
                let mut free = condition.free_variables();
                free.extend(then_branch.free_variables());
                free.extend(else_branch.free_variables());
                free
            }
            ASTNode::Literal(_) => HashMap::new(),
        }
    }

    pub fn free_types(&self) -> Vec<String> {
        match self {
            ASTNode::StructureCreation { fields, .. } => {
                fields.iter().flat_map(|(_, field)| field.free_types()).collect()
            }

            ASTNode::StructureAccess { structure, .. } => {
                let free = structure.free_types();
                free
            }

            ASTNode::Located { node, .. } => node.free_types(),
            ASTNode::Application {
                function,
                arguments,
                function_type
            } => {
                let mut free = function.free_types();
                for arg in arguments {
                    free.extend(arg.free_types());
                }

                free.extend(function_type.free());

                free
            }
            ASTNode::LetIn {
                variable,
                value,
                body,
                return_ty
            } => {
                let mut free = value.free_types();
                free.extend(body.free_types());
                free.extend(variable.value.free());
                free.extend(return_ty.free());
                free
            }
            ASTNode::Lambda {
                parameters,
                body,
                return_type,
            } => {
                let mut free = body.free_types();
                let parameters_free = parameters
                    .iter()
                    .map(|param| param.value.free())
                    .collect::<Vec<_>>()
                    .concat();
                free.extend(return_type.clone().free());
                free.extend_from_slice(&parameters_free);
                free
            }
            ASTNode::If {
                condition,
                then_branch,
                else_branch,
                return_ty,
            } => {
                let mut free = condition.free_types();
                free.extend(then_branch.free_types());
                free.extend(else_branch.free_types());
                free.extend(return_ty.free());
                free
            }
            ASTNode::Literal(_) => vec![],
            ASTNode::Identifier(ann) => ann.value.clone().free(),
        }
    }

    pub fn get_lambda(&self) -> Option<&Self> {
        match self {
            ASTNode::Lambda { .. } => Some(self),
            ASTNode::Located { node, .. } => node.get_lambda(),
            _ => None,
        }
    }
}

pub fn build_block_from_statements(statements: &[ASTNode]) -> ASTNode {
    if statements.is_empty() {
        return unit();
    }

    match statements {
        [single] => single.clone(),
        [ASTNode::Located { span, node }, rest @ ..] => {
            let mut rest_ = rest.to_vec();
            rest_.push(*node.clone());

            ASTNode::Located {
                span: span.clone(),
                node: Box::new(build_block_from_statements(&rest_)),
            }
        }
        [
            ASTNode::LetIn {
                variable,
                value,
                body,
                return_ty
            },
            rest @ ..,
        ] if body.is_unit() => ASTNode::LetIn {
            variable: variable.clone(),
            value: value.clone(),
            body: Box::new(build_block_from_statements(rest)),
            return_ty: return_ty.clone(),
        },
        [first, rest @ ..] => ASTNode::LetIn {
            variable: Annotation {
                name: "block".to_string(),
                value: None,
                location: (0, 0),
            },
            value: Box::new(first.clone()),
            body: Box::new(build_block_from_statements(rest)),
            return_ty: None,
        },
        [] => unit(),
    }
}

pub fn unit() -> ASTNode {
    ASTNode::Identifier(Annotation {
        name: vec!["unit".to_string()],
        value: None,
        location: (0, 0),
    })
}

pub fn name_mangle(name: String) -> String {
    let mut new_name = String::new();

    for c in name.chars() {
        if !c.is_alphanumeric() && c != '_' {
            new_name.push_str(&(c as u8).to_string());
        } else {
            new_name.push(c);
        }
    }
    new_name
}
