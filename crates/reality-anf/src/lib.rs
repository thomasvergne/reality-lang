use std::{cell::RefCell, collections::HashMap, rc::Rc};

use reality_ast::{
    ASTNode, TypedASTNode, TypedToplevelNode,
    llir::{LLIR, ToplevelLLIR},
};

pub struct ANF {
    pub symbol_counter: Rc<RefCell<usize>>,
}

impl ANF {
    pub fn new() -> Self {
        ANF {
            symbol_counter: Rc::new(RefCell::new(0)),
        }
    }

    pub fn compile(&mut self, ast: Vec<TypedToplevelNode>) -> Vec<ToplevelLLIR> {
        let mut anf_toplevels = Vec::new();

        for node in ast {
            if let Some(anf_node) = self.compile_toplevel_node(node) {
                anf_toplevels.push(anf_node);
            }
        }

        anf_toplevels
    }

    fn compile_toplevel_node(&mut self, node: TypedToplevelNode) -> Option<ToplevelLLIR> {
        // Implement the ANF transformation for a single top-level node
        match node {
            TypedToplevelNode::FunctionDeclaration {
                name,
                parameters,
                return_type,
                body,
            } => {
                let (anf_body, mut anf_lets) = self.compile_expr(*body);

                anf_lets.push(anf_body);

                if name.name == "main" {
                    anf_lets.insert(
                        0,
                        LLIR::Application {
                            function: Box::new(LLIR::Identifier("gc_start".into())),
                            arguments: vec![
                                LLIR::Reference(Box::new(LLIR::Identifier("gc".into()))),
                                LLIR::Reference(Box::new(LLIR::Identifier("argc".into()))),
                            ],
                        },
                    );

                    anf_lets.insert(anf_lets.len() - 1, LLIR::Application {
                        function: Box::new(LLIR::Identifier("gc_stop".into())),
                        arguments: vec![
                            LLIR::Reference(Box::new(LLIR::Identifier("gc".into()))),
                        ],
                    });
                }

                Some(ToplevelLLIR::FunctionDeclaration {
                    name: name.name,
                    parameters,
                    return_type,
                    body: anf_lets,
                })
            }

            TypedToplevelNode::ConstantDeclaration { variable, value } => {
                let (value, _) = self.compile_expr(*value);

                Some(ToplevelLLIR::ConstantDeclaration {
                    name: variable.name,
                    annotation: variable.value,
                    value: Box::new(value),
                })
            }

            TypedToplevelNode::StructureDeclaration { header, fields } => {
                Some(ToplevelLLIR::StructureDeclaration {
                    header: header.name,
                    fields,
                })
            }

            TypedToplevelNode::ExternalFunction {
                name,
                parameters,
                return_type,
            } => Some(ToplevelLLIR::ExternalFunction {
                name: name.name,
                parameters,
                return_type,
            }),

            _ => None,
        }
    }

    fn compile_expr(&mut self, node: TypedASTNode) -> (LLIR, Vec<LLIR>) {
        match node {
            ASTNode::StructureCreation {
                structure_name,
                fields,
            } => {
                let mut anf_values = Vec::new();
                let mut new_fields = HashMap::new();

                for (field_name, field_value) in fields {
                    let (anf_field_value, anf_field_let) = self.compile_expr(field_value);
                    new_fields.insert(field_name, anf_field_value);

                    anf_values.extend(anf_field_let);
                }

                let anf_structure = LLIR::StructureCreation {
                    structure_name: structure_name.name,
                    fields: new_fields,
                };

                (anf_structure, anf_values)
            }

            ASTNode::StructureAccess { structure, field } => {
                let (anf_structure, anf_let) = self.compile_expr(*structure);
                let anf_field = LLIR::StructureAccess {
                    structure: Box::new(anf_structure),
                    field,
                };
                (anf_field, anf_let)
            }

            ASTNode::Located { node, .. } => self.compile_expr(*node),

            ASTNode::Literal(value) => {
                let anf_literal = LLIR::Literal(value);
                (anf_literal, Vec::new())
            }

            ASTNode::Identifier(name) => (LLIR::Identifier(name.name), vec![]),

            ASTNode::LetIn {
                variable,
                value,
                body,
                return_ty,
            } => {
                let (anf_value, mut anf_lets) = self.compile_expr(*value);
                let (anf_body, anf_body_lets) = self.compile_expr(*body);

                let new_name = format!("let_{}", self.symbol_counter.borrow());
                self.symbol_counter.replace_with(|c| *c + 1);

                if variable.name != "_" {
                    anf_lets.push(LLIR::Let {
                        name: variable.name,
                        annotation: variable.value,
                        value: Some(Box::new(anf_value)),
                    });
                } else {
                    // If the variable is "_", we don't bind the value to a name
                    // but we still need to ensure the value is computed
                    anf_lets.push(anf_value);
                }

                for let_binding in anf_body_lets {
                    anf_lets.push(let_binding);
                }

                anf_lets.push(LLIR::Let {
                    name: new_name.clone(),
                    annotation: return_ty,
                    value: Some(Box::new(anf_body)),
                });

                (LLIR::Identifier(new_name), anf_lets)
            }

            ASTNode::Lambda { .. } => panic!("Lambda expressions are not supported"),

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
                return_ty,
            } => {
                let new_name = format!("if_{}", self.symbol_counter.borrow());
                self.symbol_counter.replace_with(|c| *c + 1);

                let (anf_condition, mut anf_lets) = self.compile_expr(*condition);
                let (anf_then, mut anf_then_lets) = self.compile_expr(*then_branch);
                let (anf_else, mut anf_else_lets) = self.compile_expr(*else_branch);

                let let_binding = LLIR::Let {
                    name: new_name,
                    annotation: return_ty,
                    value: None,
                };

                anf_then_lets.push(anf_then);
                anf_else_lets.push(anf_else);

                let anf_if = LLIR::If {
                    condition: Box::new(anf_condition),
                    then_branch: Box::new(LLIR::Block(anf_then_lets)),
                    else_branch: Box::new(LLIR::Block(anf_else_lets)),
                };

                anf_lets.push(let_binding);

                (anf_if, anf_lets)
            }

            ASTNode::Application {
                function,
                arguments,
                ..
            } => {
                let (anf_function, mut anf_lets) = self.compile_expr(*function);
                let mut anf_arguments = Vec::new();

                for arg in arguments {
                    let (anf_arg, anf_arg_lets) = self.compile_expr(arg);
                    anf_arguments.push(anf_arg);
                    anf_lets.extend(anf_arg_lets);
                }

                let anf_application = LLIR::Application {
                    function: Box::new(anf_function),
                    arguments: anf_arguments,
                };

                (anf_application, anf_lets)
            }
        }
    }
}
