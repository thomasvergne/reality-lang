// <==== ANNOTATION MODULE ====>
// 
// This module defines the Annotation struct, which represents annotated values
// in the Reality language. Annotations are used to attach metadata, such as
// names, types, and locations, to various AST nodes.
//
// <==== LITERAL MODULE ====>

use std::fmt::Debug;

#[derive(Clone, PartialEq)]
pub struct Annotation<T> {
  pub name: String,
  pub value: T,
  pub location: (usize, usize),
}

impl<T: Debug> Debug for Annotation<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)?;
    write!(f, ": {:?}", self.value)
  }
}
