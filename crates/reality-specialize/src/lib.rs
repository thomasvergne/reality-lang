use std::collections::HashMap;

use reality_ast::{
    ASTNode, ToplevelNode, TypedASTNode, TypedToplevelNode,
    internal::{annotation::Annotation, types},
};
use reality_error::RealityError;
use reality_typechecker::Typechecker;

type Scheme = types::Scheme<String>;
type Type = types::Type<String>;

pub struct Specializer<'a> {
    pub variables: HashMap<String, (Scheme, TypedToplevelNode)>,
    pub structures: HashMap<String, HashMap<String, Type>>,

    pub typechecker: &'a mut Typechecker<'a>,

    pub source: (usize, usize, String),
}

type Result<T> = std::result::Result<T, RealityError>;

impl<'a> Specializer<'a> {
    pub fn new(typechecker: &'a mut Typechecker<'a>) -> Self {
        Specializer {
            variables: HashMap::new(),
            structures: HashMap::new(),
            typechecker,
            source: (0, 0, String::new()),
        }
    }

    pub fn specialize(&mut self, ast: Vec<TypedToplevelNode>) -> Result<Vec<TypedToplevelNode>> {
        let mut specialized = Vec::new();

        for node in ast {
            if let (Some(specialized_node), ns) = self.specialize_node(node)? {
                specialized.extend(ns);
                specialized.push(specialized_node);
            }
        }

        Ok(specialized)
    }

    fn specialize_node(
        &mut self,
        node: TypedToplevelNode,
    ) -> Result<(Option<TypedToplevelNode>, Vec<TypedToplevelNode>)> {
        if let ToplevelNode::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } = node.clone()
        {
            let scheme_vars = name.clone().value;

            if scheme_vars.is_empty() {
                let (body, ns) = self.specialize_expr(*body)?;

                return Ok((
                    Some(ToplevelNode::FunctionDeclaration {
                        name,
                        parameters,
                        return_type,
                        body: Box::new(body),
                    }),
                    ns,
                ));
            } else {
                let function_args = parameters.into_iter().map(|p| p.value).collect::<Vec<_>>();

                let scheme = Scheme {
                    variables: scheme_vars,
                    body: Type::TypeFunction {
                        parameters: function_args,
                        return_type: Box::new(return_type),
                    },
                };

                self.variables.insert(name.name, (scheme, node));

                return Ok((None, vec![]));
            }
        }

        if let ToplevelNode::Located { span, node } = node {
            let old_source = self.source.clone();
            self.source = span;

            let specialized_node = self.specialize_node(*node)?;

            self.source = old_source;
            return Ok(specialized_node);
        }

        if let ToplevelNode::TypeAlias { .. }
        | ToplevelNode::ModuleDeclaration { .. }
        | ToplevelNode::ImportDeclaration(_) = node
        {
            return Ok((None, vec![]));
        }

        if let ToplevelNode::PublicDeclaration(inner) = node {
            return self.specialize_node(*inner);
        }

        if let ToplevelNode::StructureDeclaration { header, fields } = node {
            let field_map = fields
                .iter()
                .map(|(f, t)| (f.clone(), t.clone()))
                .collect::<HashMap<_, _>>();

            self.structures.insert(header.name.clone(), field_map);

            return Ok((Some(ToplevelNode::StructureDeclaration { header, fields }), vec![]));
        }

        Ok((Some(node), vec![]))
    }

    fn specialize_expr(
        &mut self,
        expr: TypedASTNode,
    ) -> Result<(TypedASTNode, Vec<TypedToplevelNode>)> {
        match expr {
            ASTNode::StructureCreation { fields, structure_name } => {
                let mut specialized_fields = HashMap::new();
                let mut ns = Vec::new();

                for (name, field) in fields {
                    let (specialized_field, n) = self.specialize_expr(field)?;
                    specialized_fields.insert(name, specialized_field);
                    ns.extend(n);
                }

                Ok((ASTNode::StructureCreation { fields: specialized_fields, structure_name }, ns))
            }

            ASTNode::StructureAccess { structure, field } => {
                let (specialized_structure, ns) = self.specialize_expr(*structure)?;
                Ok((ASTNode::StructureAccess { structure: Box::new(specialized_structure), field }, ns))
            }

            ASTNode::Literal(_) => Ok((expr, vec![])),
            ASTNode::Located { span, node } => {
                let old_source = self.source.clone();
                self.source = span;
                let specialized_node = self.specialize_expr(*node)?;
                self.source = old_source;
                Ok(specialized_node)
            }

            ASTNode::LetIn {
                variable,
                value,
                body,
            } => {
                let (specialized_value, mut n1) = self.specialize_expr(*value)?;
                let (specialized_body, n2) = self.specialize_expr(*body)?;

                n1.extend(n2);

                Ok((
                    ASTNode::LetIn {
                        variable,
                        value: Box::new(specialized_value),
                        body: Box::new(specialized_body),
                    },
                    n1,
                ))
            }

            ASTNode::Lambda {
                parameters,
                return_type,
                body,
            } => {
                let (specialized_body, ns) = self.specialize_expr(*body)?;
                Ok((
                    ASTNode::Lambda {
                        parameters,
                        return_type,
                        body: Box::new(specialized_body),
                    },
                    ns,
                ))
            }

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (specialized_condition, mut n1) = self.specialize_expr(*condition)?;
                let (specialized_then, n2) = self.specialize_expr(*then_branch)?;
                let (specialized_else, n3) = self.specialize_expr(*else_branch)?;

                n1.extend(n2);
                n1.extend(n3);

                Ok((
                    ASTNode::If {
                        condition: Box::new(specialized_condition),
                        then_branch: Box::new(specialized_then),
                        else_branch: Box::new(specialized_else),
                    },
                    n1,
                ))
            }

            ASTNode::Identifier(name) => {
                if let Some(scheme) = self.variables.get(&name.name) {
                    let (specialized, ns) = self.specialize_identifier(name, scheme.clone())?;

                    return Ok((ASTNode::Identifier(specialized), ns));
                }

                return Ok((ASTNode::Identifier(name), vec![]));
            }

            ASTNode::Application {
                function,
                arguments,
                function_type
            } => {
                let (specialized_function, mut n1) = self.specialize_expr(*function)?;

                let (specialized_arguments, n2) = arguments
                    .into_iter()
                    .map(|arg| self.specialize_expr(arg))
                    .collect::<Result<(Vec<_>, Vec<_>)>>()?;

                let n2_concatenated = n2.into_iter().flatten().collect::<Vec<_>>();
                n1.extend(n2_concatenated);

                Ok((
                    ASTNode::Application {
                        function: Box::new(specialized_function),
                        arguments: specialized_arguments,
                        function_type
                    },
                    n1,
                ))
            }
        }
    }

    fn specialize_identifier(
        &mut self,
        name: Annotation<Type>,
        function: (Scheme, TypedToplevelNode),
    ) -> Result<(Annotation<Type>, Vec<TypedToplevelNode>)> {
        let (scheme, node) = function;
        let (ty, subst) = self.typechecker.instantiate_and_sub(scheme);

        self.typechecker.is_subtype(name.value, ty.clone())?;

        let mut new_subst = HashMap::new();

        if let ToplevelNode::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        } = node
        {
            let scheme_vars = name.clone().value;

            for (var, ty) in subst {
                if scheme_vars.contains(&var) {
                    new_subst.insert(var, ty);
                }
            }

            let subst_as_vec = new_subst
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect::<Vec<_>>();

            let new_parameters = parameters
                .into_iter()
                .map(|param| Annotation {
                    name: param.name,
                    value: param.value.substitute_all(subst_as_vec.clone()),
                    location: param.location,
                })
                .collect::<Vec<_>>();

            let new_return_type = return_type.substitute_all(subst_as_vec);

            let new_body = self.apply_sub(*body, &new_subst);

            let new_name = format!("{}_{}", name.name, new_subst.values().map(|t| t.to_string()).collect::<Vec<_>>().join("_"));

            return Ok((
                Annotation {
                    name: new_name.clone(),
                    value: ty,
                    location: name.location,
                },
                vec![ToplevelNode::FunctionDeclaration {
                    name: Annotation {
                        name: new_name,
                        value: vec![],
                        location: name.location,
                    },
                    parameters: new_parameters,
                    return_type: new_return_type,
                    body: Box::new(new_body),
                }],
            ));
        }

        Ok((
            Annotation {
                name: name.name,
                value: ty,
                location: name.location,
            },
            vec![],
        ))
    }

    fn apply_sub(&mut self, expr: TypedASTNode, subst: &HashMap<String, Type>) -> TypedASTNode {
        match expr {
            ASTNode::StructureCreation { fields, structure_name } => {
                let specialized_fields = fields
                    .into_iter()
                    .map(|(name, field)| (name, self.apply_sub(field, subst)))
                    .collect();

                ASTNode::StructureCreation { fields: specialized_fields, structure_name }
            }

            ASTNode::StructureAccess { structure, field } => {
                let specialized_structure = self.apply_sub(*structure, subst);
                ASTNode::StructureAccess {
                    structure: Box::new(specialized_structure),
                    field,
                }
            }

            ASTNode::Literal(_) => expr,
            ASTNode::Located { span, node } => {
                let specialized_node = self.apply_sub(*node, subst);
                ASTNode::Located {
                    span,
                    node: Box::new(specialized_node),
                }
            }

            ASTNode::LetIn {
                variable,
                value,
                body,
            } => {
                let subst_as_vec = subst
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>();

                let specialized_value = self.apply_sub(*value, subst);
                let specialized_body = self.apply_sub(*body, subst);

                let new_variable = Annotation {
                    name: variable.name,
                    value: variable.value.substitute_all(subst_as_vec.clone()),
                    location: variable.location,
                };

                ASTNode::LetIn {
                    variable: new_variable,
                    value: Box::new(specialized_value),
                    body: Box::new(specialized_body),
                }
            }

            ASTNode::Lambda {
                parameters,
                return_type,
                body,
            } => {
                let subst_as_vec = subst
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>();

                let new_parameters = parameters
                    .into_iter()
                    .map(|param| Annotation {
                        name: param.name,
                        value: param.value.substitute_all(subst_as_vec.clone()),
                        location: param.location,
                    })
                    .collect::<Vec<_>>();

                let new_return_type = return_type.substitute_all(subst_as_vec.clone());

                let specialized_body = self.apply_sub(*body, subst);
                ASTNode::Lambda {
                    parameters: new_parameters,
                    return_type: new_return_type,
                    body: Box::new(specialized_body),
                }
            }

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let specialized_condition = self.apply_sub(*condition, subst);
                let specialized_then = self.apply_sub(*then_branch, subst);
                let specialized_else = self.apply_sub(*else_branch, subst);

                ASTNode::If {
                    condition: Box::new(specialized_condition),
                    then_branch: Box::new(specialized_then),
                    else_branch: Box::new(specialized_else),
                }
            }

            ASTNode::Identifier(name) => {
                let mut new_type = name.value.clone();

                for (var, ty) in subst {
                    new_type = new_type.substitute(var, ty.clone());
                }

                ASTNode::Identifier(Annotation {
                    name: name.name,
                    value: new_type,
                    location: name.location,
                })
            }

            ASTNode::Application {
                function,
                arguments,
                function_type
            } => {
                let specialized_function = self.apply_sub(*function, subst);

                let subst_as_vec = subst
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>();

                let specialized_function_type = function_type.substitute_all(subst_as_vec.clone());

                let specialized_arguments = arguments
                    .into_iter()
                    .map(|arg| self.apply_sub(arg, subst))
                    .collect::<Vec<_>>();

                ASTNode::Application {
                    function: Box::new(specialized_function),
                    arguments: specialized_arguments,
                    function_type: specialized_function_type,
                }
            }
        }
    }
}
