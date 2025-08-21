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

use std::{cell::RefCell, rc::Rc, fmt::{Debug, Display}};

#[derive(Clone, PartialEq)]
pub enum Type<N = Vec<String>> {
    TypeIdentifier(N),
    TypeApplication(Box<Type<N>>, Vec<Type<N>>),
    TypeVariable(Rc<RefCell<TypeVariable<N>>>),
    TypeFunction {
        parameters: Vec<Type<N>>,
        return_type: Box<Type<N>>,
    },
    TypeAliased(Box<Type<N>>, String),
}

#[derive(Clone, PartialEq)]
pub enum TypeVariable<N = Vec<String>> {
    Unbound(String, usize),
    Bound(Box<Type<N>>),
}

#[derive(Clone, PartialEq)]
pub struct Scheme<N = Vec<String>> {
    pub variables: Vec<String>,
    pub body: Type<N>,
}

impl Type<String> {
    pub fn is_integer_type(&self) -> Option<i8> {
        match self {
            Type::TypeIdentifier(name) if name.starts_with("i") => {
                let size = name[1..].parse::<i8>().ok()?;
                Some(size)
            }
            _ => None,
        }
    }

    pub fn is_unsigned_integer_type(&self) -> Option<i8> {
        match self {
            Type::TypeIdentifier(name) if name.starts_with("u") => {
                let size = name[1..].parse::<i8>().ok()?;
                Some(size)
            }
            _ => None,
        }
    }

    pub fn is_floating_point_type(&self) -> Option<i8> {
        match self {
            Type::TypeIdentifier(name) if name.starts_with("f") => {
                let size = name[1..].parse::<i8>().ok()?;
                Some(size)
            }
            _ => None,
        }
    }
}

impl Type<Vec<String>> {
    pub fn normalize(&self) -> Type<String> {
        match self {
            Type::TypeIdentifier(name) => Type::TypeIdentifier(name.join("::")),
            Type::TypeApplication(base, args) => {
                let base = base.normalize();
                let args = args.iter().map(|arg| arg.normalize()).collect();
                Type::TypeApplication(Box::new(base), args)
            }
            Type::TypeVariable(var) => match &*var.borrow() {
                TypeVariable::Bound(ty) => ty.normalize(),
                TypeVariable::Unbound(name, id) => {
                    Type::TypeVariable(Rc::new(RefCell::new(TypeVariable::Unbound(name.clone(), *id))))
                }
            },
            Type::TypeFunction {
                parameters,
                return_type,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|param| param.normalize())
                    .collect();

                let return_type = return_type.normalize();

                Type::TypeFunction {
                    parameters,
                    return_type: Box::new(return_type),
                }
            }

            Type::TypeAliased(inner, name) => {
                let inner = inner.normalize();
                Type::TypeAliased(Box::new(inner), name.clone())
            }
        }
    }
}

impl<N: Debug> Debug for TypeVariable<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeVariable::Unbound(name, id) => write!(f, "{}{}", name, id),
            TypeVariable::Bound(ty) => write!(f, "{:?}", ty),
        }
    }
}

impl<N: Display> Display for TypeVariable<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeVariable::Unbound(name, id) => write!(f, "{}{}", name, id),
            TypeVariable::Bound(ty) => write!(f, "{}", ty),
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
            Type::TypeVariable(var) => write!(f, "{:?}", *var.borrow()),
            Type::TypeFunction {
                parameters,
                return_type,
            } => {
                write!(f, "fn(")?;
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", param)?;
                }
                write!(f, ") -> {:?}", return_type)
            }

            Type::TypeAliased(inner, name) => {
                write!(f, "{} (resp. {:?})", name, inner)
            }
        }
    }
}

impl<N: Display> Display for Type<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::TypeIdentifier(name) => write!(f, "{}", name),
            Type::TypeApplication(base, args) => {
                write!(f, "{}<", base)?;
                for (i, arg) in args.iter().enumerate() {
                    write!(f, "{}", arg)?;
                    if i < args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ">")
            }
            Type::TypeVariable(var) => write!(f, "{}", var.borrow()),
            Type::TypeFunction {
                parameters,
                return_type,
            } => {
                write!(f, "fn(")?;
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", return_type)
            }

            Type::TypeAliased(inner, name) => {
                write!(f, "{} (resp. {})", name, inner)
            }
        }
    }
}

impl<N: Debug> Debug for Scheme<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "forall ")?;
        for (i, var) in self.variables.iter().enumerate() {
            write!(f, "{}", var)?;
            if i < self.variables.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ". {:?}", self.body)
    }
}

impl<N: Display> Display for Scheme<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "forall ")?;
        for (i, var) in self.variables.iter().enumerate() {
            write!(f, "{}", var)?;
            if i < self.variables.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ". {}", self.body)
    }
}

impl Type<String> {
    pub fn substitute<'a>(&self, name: &'a str, replacement: Self) -> Self {
        match self {
            Type::TypeIdentifier(id) => if id == name {
                replacement
            } else {
                self.clone()
            },
            Type::TypeApplication(base, args) => Type::TypeApplication(
                Box::new(base.substitute(name, replacement.clone())),
                args.iter()
                    .map(|arg| arg.substitute(name, replacement.clone()))
                    .collect(),
            ),
            Type::TypeVariable(var) => {
                if let TypeVariable::Bound(ty) = &*var.borrow() {
                    return ty.substitute(name, replacement);
                }

                self.clone()
            }
            Type::TypeFunction {
                parameters,
                return_type,
            } => Type::TypeFunction {
                parameters: parameters
                    .iter()
                    .map(|param| param.substitute(name, replacement.clone()))
                    .collect(),
                return_type: Box::new(return_type.substitute(name, replacement.clone())),
            },

            Type::TypeAliased(inner, name) => {
                Type::TypeAliased(Box::new(inner.substitute(name, replacement)), name.clone())
            }
        }
    }

    pub fn substitute_all<'a>(&self, substitutions: Vec<(String, Self)>) -> Self {
        match self {
            Type::TypeIdentifier(name) => {
                if let Some((_, replacement)) = substitutions.iter().find(|(n, _)| n == name) {
                    return replacement.clone();
                }
                self.clone()
            }
            Type::TypeApplication(base, args) => {
                let new_args = args
                    .iter()
                    .map(|arg| arg.substitute_all(substitutions.clone()))
                    .collect();
                Type::TypeApplication(Box::new(base.substitute_all(substitutions)), new_args)
            }
            Type::TypeVariable(var) => {
                if let TypeVariable::Bound(ty) = &*var.borrow() {
                    return ty.substitute_all(substitutions);
                }
                self.clone()
            }
            Type::TypeFunction {
                parameters,
                return_type,
            } => {
                let new_params = parameters
                    .iter()
                    .map(|param| param.substitute_all(substitutions.clone()))
                    .collect();
                Type::TypeFunction {
                    parameters: new_params,
                    return_type: Box::new(return_type.substitute_all(substitutions)),
                }
            }

            Type::TypeAliased(inner, name) => {
                Type::TypeAliased(Box::new(inner.substitute_all(substitutions)), name.clone())
            }
        }
    }
}
