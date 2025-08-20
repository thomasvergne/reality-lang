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

pub enum ToplevelNode<T> {
  ConstantDeclaration {
    variable: Annotation<T>,
    value: Box<ASTNode<T>>,
  },

  FunctionDeclaration {
    name: Annotation<T>,
    generics: Vec<String>,
    parameters: Vec<Annotation<T>>,
    return_type: T,
    body: Box<ASTNode<T>>,
  },

  Require(String),

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
      ToplevelNode::FunctionDeclaration { name, generics, parameters, return_type, body } => {
        write!(f, "fn {:?}<{:?}>(", name, generics)?;
        for param in parameters {
          write!(f, "{:?}, ", param)?;
        }
        write!(f, ") -> {:?} {{ {:?} }}", return_type, body)
      }
      ToplevelNode::Require(module) => write!(f, "require \"{}\";", module),
      ToplevelNode::TypeAlias { name, body } => write!(f, "type {:?} = {:?};", name, body),
    }
  }
}

