use std::collections::HashMap;

use crate::internal::{annotation::Annotation, literal::Literal, types::Type};

#[derive(Debug, Clone)]
pub enum LLIR {
    Let {
        name: String,
        annotation: Type<String>,
        value: Option<Box<LLIR>>
    },
    Literal(Literal),
    Identifier(String),
    Application {
        function: Box<LLIR>,
        arguments: Vec<LLIR>,
    },

    If {
        condition: Box<LLIR>,
        then_branch: Box<LLIR>,
        else_branch: Box<LLIR>,
    },

    StructureAccess {
        structure: Box<LLIR>,
        field: String,
    },
    StructureCreation {
        structure_name: String,
        fields: HashMap<String, LLIR>,
    },

    Block(Vec<LLIR>)
}

#[derive(Debug, Clone)]
pub enum ToplevelLLIR {
    ConstantDeclaration {
        name: String,
        annotation: Type<String>,
        value: Box<LLIR>,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<Annotation<Type<String>>>,
        return_type: Type<String>,
        body: Vec<LLIR>,
    },

    StructureDeclaration {
        header: String,
        fields: HashMap<String, Type<String>>,
    }
}
