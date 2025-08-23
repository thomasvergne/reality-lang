use std::{cell::RefCell, rc::Rc};

use reality_ast::{internal::{annotation::Annotation, types::Type}, ASTNode, TypedASTNode, TypedToplevelNode};

pub struct Hoister {
    pub symbol_counter: Rc<RefCell<usize>>,
}

impl Hoister {
    pub fn new() -> Self {
        Hoister {
            symbol_counter: Rc::new(RefCell::new(0)),
        }
    }

    pub fn next_symbol(&self) -> usize {
        let current = self.symbol_counter.borrow().clone();
        *self.symbol_counter.borrow_mut() += 1;
        current
    }

    pub fn hoist(&self, ast: Vec<TypedToplevelNode>) -> Vec<TypedToplevelNode> {
        let mut hoisted_toplevels = Vec::new();

        for node in ast {
            let (hoisted_node, captured) = self.hoist_node(node);
            hoisted_toplevels.extend(captured);
            hoisted_toplevels.push(hoisted_node);
        }

        hoisted_toplevels
    }

    fn hoist_node(&self, node: TypedToplevelNode) -> (TypedToplevelNode, Vec<TypedToplevelNode>) {
        match node {
            TypedToplevelNode::FunctionDeclaration { name, parameters, body, return_type } => {
                let (new_body, captured) = self.hoist_expr(*body);
                
                (TypedToplevelNode::FunctionDeclaration { name, parameters, body: Box::new(new_body), return_type }, captured)
            }
            
            TypedToplevelNode::Located { span, node } => {
                let (new_node, captured) = self.hoist_node(*node);
                (TypedToplevelNode::Located { span, node: Box::new(new_node) }, captured)
            }

            _ => (node, Vec::new()),
        }
    }

    fn hoist_expr(&self, expr: TypedASTNode) -> (TypedASTNode, Vec<TypedToplevelNode>) {
        match expr {
            ASTNode::Application { function, arguments, function_type } => {
                let (new_function, captured1) = self.hoist_expr(*function);
                let (new_arguments, captured2): (Vec<_>, Vec<_>) = arguments.into_iter().map(|arg| self.hoist_expr(arg)).unzip();

                let captured2_concatenated = captured2.concat();

                (ASTNode::Application { function: Box::new(new_function), arguments: new_arguments, function_type }, [captured1, captured2_concatenated].concat())
            }

            ASTNode::Identifier(name) => {
                (ASTNode::Identifier(name), vec![])
            }

            ASTNode::If { condition, then_branch, else_branch, return_ty } => {
                let (new_condition, captured1) = self.hoist_expr(*condition);
                let (new_then_branch, captured2) = self.hoist_expr(*then_branch);
                let (new_else_branch, captured3) = self.hoist_expr(*else_branch);

                let captured = [captured1, captured2, captured3].concat();

                (ASTNode::If { condition: Box::new(new_condition), then_branch: Box::new(new_then_branch), else_branch: Box::new(new_else_branch), return_ty }, captured)
            }

            ASTNode::StructureCreation { structure_name, fields } => {
                let (hoisted_fields, captured): (_, Vec<_>) = fields.into_iter().map(|(field, field_expr)| {
                    let (new_field_expr, captured) = self.hoist_expr(field_expr);
                    ((field, new_field_expr), captured)
                }).unzip();

                (ASTNode::StructureCreation { structure_name, fields: hoisted_fields }, captured.concat())
            }

            ASTNode::StructureAccess { structure, field } => {
                let (new_structure, captured) = self.hoist_expr(*structure);

                (ASTNode::StructureAccess { structure: Box::new(new_structure), field }, captured)
            }

            ASTNode::Located { span, node } => {
                let (new_node, captured) = self.hoist_expr(*node);
                (ASTNode::Located { span, node: Box::new(new_node) }, captured)
            }

            ASTNode::Literal(value) => {
                (ASTNode::Literal(value), vec![])
            }

            ASTNode::LetIn { variable, value, body, return_ty } => {
                let (new_value, captured1) = self.hoist_expr(*value);
                let (new_body, captured2) = self.hoist_expr(*body);
                let captured = [captured1, captured2].concat();

                (ASTNode::LetIn { variable, value: Box::new(new_value), body: Box::new(new_body), return_ty }, captured)
            }

            ASTNode::Lambda { parameters, return_type, body } => {
                let (new_body, mut captured) = self.hoist_expr(*body);
                
                let new_name = format!("lambda_{}", self.next_symbol());

                let param_types = parameters.iter().map(|param| param.value.clone()).collect();
                let function_type = Type::TypeFunction {
                    parameters: param_types,
                    return_type: Box::new(return_type.clone()),
                };

                let new_function = vec![TypedToplevelNode::FunctionDeclaration { name: Annotation {
                    name: new_name.clone(),
                    location: (0, 0),
                    value: vec![]
                }, parameters: parameters, return_type: return_type, body: Box::new(new_body) }];

                captured.extend(new_function);

                (ASTNode::Identifier(Annotation {
                    name: new_name,
                    location: (0, 0),
                    value: function_type
                }), captured)
            }
        }
    }
}
