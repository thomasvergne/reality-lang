// <==== LITERAL MODULE ====>
// 
// This module defines the Literal enum, which represents various literal values.
// Literals are the most basic building blocks of Reality language, including
// numbers, strings, characters, and more.
// 
// Each variant of the Literal enum corresponds to a specific type of literal value:
// 
// (1). Integer(i64) may be represented by `50`, `0`, `-10`, `42`.
// 
// (2). Float(f64) may be represented by `50.6`, `-23.4`, `.5`, `5.`.
// 
// (3). String(String) may be represented by `"hello"`, `""`, `"world"`.
// 
// (4). Character(char) may be represented by `'a'`, `'Z'`, `'\n'`.
//
// (5). Boolean(bool) may be represented by `true`, `false`.
// 
// <==== LITERAL MODULE ====>

use std::fmt::Debug;

#[derive(Clone, PartialEq)]
pub enum Literal {
  Integer(i64),
  Float(f64),
  String(String),
  Character(char),
  Boolean(bool),
}

impl Debug for Literal {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Literal::Integer(i) => write!(f, "{}", i),
      Literal::Float(fl) => write!(f, "{}", fl),
      Literal::String(s) => write!(f, "{:?}", s),
      Literal::Character(c) => write!(f, "{:?}", c),
      Literal::Boolean(b) => write!(f, "{}", b),
    }
  }
}

