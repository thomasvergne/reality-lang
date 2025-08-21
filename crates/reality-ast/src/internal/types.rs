// <==== TYPES MODULE ====>
// 
// This module defines the various types used in the Reality language.
// Whereas values represent the building blocks of the language, types
// provide the structure and constraints for those values, ensuring
// that they are used correctly within the language's syntax and semantics.
// 
// The following types are defined by default in the Reality language:
// 
// (1). TypeIdentifier(String) may be represented by <id>.
// 
// (2). TypeApplication(Box<Type>, Vec<Type>) may be represented by 
//      <base_type>[<arg1>, <arg2>, ...].
// 
// (3). TypeVariable(Box<TypeVariable>) may not be represented in source code. It
//      represents a type variable that can be instantiated with different types.
// 
// (4). TypeFunction { parameters: Vec<Type>, return_type: Box<Type> } 
//      may be represented by fn(<param1>, <param2>, ...) -> <return_type>.
// 
// (5). TypeQuantified(String) may be represented by <name>, representing a type
//      that is universally quantified by function generics.
//
// <==== TYPES MODULE ====>

use std::fmt::Debug;

#[derive(Clone, PartialEq)]
pub enum Type<N = Vec<String>> {
    TypeIdentifier(N),
    TypeApplication(Box<Type>, Vec<Type>),
    TypeVariable(Box<TypeVariable<N>>),
    TypeFunction {
      parameters: Vec<Type>,
      return_type: Box<Type>,
    },
    TypeQuantified(String),
}

#[derive(Clone, PartialEq)]
pub enum TypeVariable<N = Vec<String>> {
    Unbound(String, usize),
    Bound(Box<Type<N>>),
}

impl<N: Debug> Debug for TypeVariable<N> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
          TypeVariable::Unbound(name, id) => write!(f, "{}{}", name, id),
          TypeVariable::Bound(ty) => write!(f, "{:?}", ty),
      }
  }
}

impl<N: Debug> Debug for Type<N> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
      match self {
          Type::TypeIdentifier(name) => write!(f, "{:?}", name),
          Type::TypeApplication(base, args) => {
            write!(f, "{:?}<", base)?;
            for (i, arg) in args.iter().enumerate() {
                write!(f, "{:?}", arg)?;
                if i < args.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ">")
          }
          Type::TypeVariable(var) => write!(f, "{:?}", var),
          Type::TypeFunction { parameters, return_type } => {
            write!(f, "fn(")?;
            for (i, param) in parameters.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", param)?;
            }
            write!(f, ") -> {:?}", return_type)
          }
          Type::TypeQuantified(name) => write!(f, "{}", name),
      }
  }
}
