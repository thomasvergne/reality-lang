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

use std::fmt::Debug;

use crate::internal::{annotation::Annotation, literal::Literal, types::Type};

pub mod internal;

#[derive(Clone, PartialEq)]
pub enum ASTNode<T = Option<Type>> {
  Literal(Literal),
  Identifier(Annotation<T>),

  Application {
    function: Box<ASTNode<T>>,
    arguments: Vec<ASTNode<T>>,
  },

  Lambda {
    parameters: Vec<Annotation<T>>,
    return_type: T,
    body: Box<ASTNode<T>>,
  },

  LetIn {
    variable: Annotation<T>,
    value: Box<ASTNode<T>>,
    body: Box<ASTNode<T>>,
  },

  If {
    condition: Box<ASTNode<T>>,
    then_branch: Box<ASTNode<T>>,
    else_branch: Box<ASTNode<T>>,
  },

  Located {
    span: (usize, usize),
    node: Box<ASTNode<T>>,
  }
}

#[derive(Clone, PartialEq)]
pub enum ToplevelNode<T = Type, AT = Option<Type>> {
  ConstantDeclaration {
    variable: Annotation<AT>,
    value: Box<ASTNode<AT>>,
  },

  FunctionDeclaration {
    name: Annotation<Vec<String>>,
    parameters: Vec<Annotation<T>>,
    return_type: T,
    body: Box<ASTNode<AT>>,
  },

  ImportDeclaration(String),

  TypeAlias {
    name: Annotation<Vec<String>>,
    body: Type,
  },
}

impl<T: Debug> Debug for ASTNode<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ASTNode::Literal(lit) => write!(f, "{:?}", lit),
      ASTNode::Identifier(id) => write!(f, "{:?}", id),
      ASTNode::Application { function, arguments } => {
        write!(f, "({:?}(", function)?;
        for arg in arguments {
          write!(f, "{:?}, ", arg)?;
        }
        write!(f, "))")
      }
      ASTNode::Lambda { parameters, return_type, body } => {
        write!(f, "fn(")?;
        for param in parameters {
          write!(f, "{:?}, ", param)?;
        }
        write!(f, ") -> {:?} {{ {:?} }}", return_type, body)
      }
      ASTNode::LetIn { variable, value, body } => {
        write!(f, "let {:?} = {:?}; {:?}", variable, value, body)
      }
      ASTNode::If { condition, then_branch, else_branch } => {
        write!(f, "if {:?} {{ {:?} }} else {{ {:?} }}", condition, then_branch, else_branch)
      }
      ASTNode::Located { span, node } => write!(f, "Located({:?}, {:?})", span, node),
    }
  }
}

impl<T: Debug> Debug for ToplevelNode<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ToplevelNode::ConstantDeclaration { variable, value } => {
        write!(f, "const {:?} = {:?};", variable, value)
      }
      ToplevelNode::FunctionDeclaration { name, parameters, return_type, body } => {
        write!(f, "fn {}<{:?}>(", name.name, name.value)?;
        for param in parameters {
          write!(f, "{:?}, ", param)?;
        }
        write!(f, ") -> {:?} {{ {:?} }}", return_type, body)
      }
      ToplevelNode::ImportDeclaration(module) => write!(f, "require \"{}\";", module),
      ToplevelNode::TypeAlias { name, body } => write!(f, "type {:?} = {:?};", name, body),
    }
  }
}

impl<T: Clone> ASTNode<T> {
    pub fn located(self, span: (usize, usize)) -> Self {
        ASTNode::Located { span, node: Box::new(self) }
    }

    pub fn is_unit(&self) -> bool {
        matches!(self, ASTNode::Identifier(Annotation { name, .. }) if name == "unit")
    }


    pub fn flatten_locations(&self) -> ASTNode<T> {
        match self {
            ASTNode::Located { node, .. } => node.flatten_locations(),
            ASTNode::Application { function, arguments } => ASTNode::Application {
                function: Box::new(function.flatten_locations()),
                arguments: arguments
                    .iter()
                    .map(ASTNode::flatten_locations)
                    .collect(),
            },
            ASTNode::LetIn {
                variable,
                value,
                body,
            } => ASTNode::LetIn {
                variable: variable.clone(),
                value: Box::new(value.flatten_locations()),
                body: Box::new(body.flatten_locations()),
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
            } => ASTNode::If {
                condition: Box::new(condition.flatten_locations()),
                then_branch: Box::new(then_branch.flatten_locations()),
                else_branch: Box::new(else_branch.flatten_locations()),
            },
            ASTNode::Literal(literal) => ASTNode::Literal(literal.clone()),
            ASTNode::Identifier(ann) => ASTNode::Identifier(ann.clone())
        }
    }
}



pub fn build_block_from_statements(statements: &[ASTNode]) -> ASTNode {
    if statements.is_empty() {
        return unit();
    }

    match statements
        .iter()
        .map(ASTNode::flatten_locations)
        .collect::<Vec<_>>()
        .as_slice()
    {
        [single] => single.clone(),
        [ASTNode::LetIn {
            variable,
            value,
            body,
        }, rest @ ..]
            if body.is_unit() =>
        {
            ASTNode::LetIn {
                variable: variable.clone(),
                value: value.clone(),
                body: Box::new(build_block_from_statements(rest)),
            }
        }
        [first, rest @ ..] => ASTNode::LetIn {
            variable: Annotation {
                name: "block".to_string(),
                value: None,
                location: (0, 0),
            },
            value: Box::new(first.clone()),
            body: Box::new(build_block_from_statements(rest)),
        },
        [] => unit(),
    }
}

pub fn unit() -> ASTNode {
    ASTNode::Identifier(Annotation {
        name: "unit".to_string(),
        value: None,
        location: (0, 0),
    })
}
