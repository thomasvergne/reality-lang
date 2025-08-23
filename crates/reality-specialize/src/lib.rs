use std::collections::HashMap;

use reality_ast::{
    ASTNode, ToplevelNode, TypedASTNode, TypedToplevelNode,
    internal::{
        annotation::Annotation,
        types::{self, TypeVariable},
    },
};
use reality_error::{Generics, RealityError};
use reality_typechecker::Typechecker;

type Scheme = types::Scheme<Type>;
type Type = types::Type<String>;

pub struct Specializer<'a> {
    pub variables: HashMap<String, (Scheme, TypedToplevelNode)>,
    pub structures: HashMap<String, (Vec<String>, HashMap<String, Type>)>,
    pub properties: HashMap<String, (Scheme, TypedToplevelNode)>,
    pub implementations: Vec<((String, Scheme), TypedToplevelNode)>,

    pub already_converted_variables: Vec<String>,
    pub already_converted_structures: Vec<String>,
    pub already_converted_implementations: Vec<String>,

    pub typechecker: &'a mut Typechecker<'a>,

    pub source: (usize, usize, String),
}

type Result<T> = std::result::Result<T, RealityError>;

impl<'a> Specializer<'a> {
    pub fn new(typechecker: &'a mut Typechecker<'a>) -> Self {
        Specializer {
            variables: HashMap::new(),
            structures: HashMap::new(),
            implementations: Vec::new(),
            properties: HashMap::new(),

            typechecker,
            source: (0, 0, String::new()),

            already_converted_variables: Vec::new(),
            already_converted_structures: Vec::new(),
            already_converted_implementations: Vec::new(),
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

                let return_type = self.specialize_type(return_type.clone());
                let parameters = parameters
                    .into_iter()
                    .map(|p| Annotation {
                        location: p.location,
                        name: p.name,
                        value: self.specialize_type(p.value.clone()),
                    })
                    .collect::<Vec<_>>();

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

        if let ToplevelNode::Property {
            ref header,
            ref value,
        } = node
        {
            let scheme = Scheme {
                variables: header.value.clone(),
                body: value.clone(),
            };

            self.properties.insert(header.name.clone(), (scheme, node));

            return Ok((None, vec![]));
        }

        if let ToplevelNode::Implementation {
            for_type,
            header,
            mut arguments,
            return_type,
            body,
        } = node
        {
            arguments.insert(0, for_type.clone());

            let function = ToplevelNode::FunctionDeclaration {
                name: Annotation {
                    location: header.location,
                    name: header.name.clone(),
                    value: header.value.clone(),
                },
                parameters: arguments.clone(),
                return_type: return_type.clone(),
                body: body,
            };

            let type_function = Type::TypeFunction {
                parameters: arguments.iter().map(|x| x.value.clone()).collect(),
                return_type: Box::new(return_type.clone()),
            };

            self.implementations.push((
                (
                    header.name.clone(),
                    Scheme {
                        body: type_function,
                        variables: header.value.clone(),
                    },
                ),
                function,
            ));

            return Ok((None, vec![]));
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
            if header.value.len() > 0 {
                let field_map = fields
                    .iter()
                    .map(|(f, t)| (f.clone(), t.clone()))
                    .collect::<HashMap<_, _>>();

                self.structures
                    .insert(header.name.clone(), (header.value.clone(), field_map));

                return Ok((None, vec![]));
            } else {
                let field_map = fields
                    .iter()
                    .map(|(f, t)| (f.clone(), self.specialize_type(t.clone())))
                    .collect::<HashMap<_, _>>();

                self.already_converted_structures
                    .push(header.name.clone());
                self.structures
                    .insert(header.name.clone(), (header.value.clone(), field_map));

                return Ok((
                    Some(ToplevelNode::StructureDeclaration { header, fields }),
                    vec![],
                ));
            }
        }

        Ok((Some(node), vec![]))
    }

    fn specialize_structure(
        &mut self,
        ann: Type,
        name: String,
    ) -> Result<(String, Type, Vec<TypedToplevelNode>)> {
        let struct_type = ann.clone();

        let args = if let Type::TypeApplication(_, args) = struct_type.clone() {
            args
        } else {
            vec![]
        };

        let mut subst = HashMap::new();

        let structures = self.structures.clone();

        if let Some((scheme_vars, hm)) = structures.get(&name) {
            if scheme_vars.len() != args.len() {
                return Err(RealityError::WrongNumberOfTypeArguments {
                    expected: scheme_vars.len(),
                    found: args.len(),
                });
            }

            for (var, ty) in scheme_vars.iter().zip(args.iter()) {
                subst.insert(var.clone(), ty.clone());
            }

            let subst_as_vec = subst
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect::<Vec<_>>();

            let specialized_fields = hm
                .iter()
                .map(|(f, t)| (f.clone(), t.substitute_all(subst_as_vec.clone())))
                .collect::<HashMap<_, _>>();

            let mut new_hm = HashMap::new();

            for (f, t) in specialized_fields.clone() {
                new_hm.insert(f, self.specialize_type(t));
            }

            let new_name = if args.len() == 0 {
                name.clone()
            } else {
                format!(
                    "{}_{}",
                    name,
                    args.iter()
                        .map(|a| format!("{}", self.remove_alias(a.clone())))
                        .collect::<Vec<_>>()
                        .join("_")
                )
            };
            self.already_converted_structures.push(new_name.clone());

            let ns = if !self.already_converted_structures.contains(&new_name) {
                vec![ToplevelNode::StructureDeclaration {
                    header: Annotation {
                        location: (0, 0),
                        name: new_name.clone(),
                        value: vec![],
                    },
                    fields: specialized_fields,
                }]
            } else {
                vec![]
            };

            self.structures.insert(new_name.clone(), (vec![], new_hm));

            Ok((
                new_name.clone(),
                Type::TypeIdentifier(new_name.clone()),
                ns,
            ))
        } else {
            return Err(RealityError::UnknownStructure(name));
        }
    }

    fn specialize_expr(
        &mut self,
        expr: TypedASTNode,
    ) -> Result<(TypedASTNode, Vec<TypedToplevelNode>)> {
        match expr {
            ASTNode::StructureCreation {
                fields,
                structure_name,
            } => {
                let mut specialized_fields = HashMap::new();
                let mut ns = Vec::new();

                let (name, ty, structure) =
                    self.specialize_structure(structure_name.value, structure_name.name)?;

                ns.extend(structure);

                for (name, field) in fields {
                    let (specialized_field, n) = self.specialize_expr(field)?;
                    specialized_fields.insert(name, specialized_field);
                    ns.extend(n);
                }

                Ok((
                    ASTNode::StructureCreation {
                        fields: specialized_fields,
                        structure_name: Annotation {
                            name: name.clone(),
                            value: ty,
                            location: structure_name.location,
                        },
                    },
                    ns,
                ))
            }

            ASTNode::StructureAccess { structure, field } => {
                let (specialized_structure, ns) = self.specialize_expr(*structure)?;
                Ok((
                    ASTNode::StructureAccess {
                        structure: Box::new(specialized_structure),
                        field,
                    },
                    ns,
                ))
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
                return_ty,
            } => {
                let (specialized_value, mut n1) = self.specialize_expr(*value)?;
                let (specialized_body, n2) = self.specialize_expr(*body)?;

                n1.extend(n2);

                Ok((
                    ASTNode::LetIn {
                        variable,
                        value: Box::new(specialized_value),
                        body: Box::new(specialized_body),
                        return_ty: return_ty.clone(),
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

                let return_type = self.specialize_type(return_type.clone());
                let args = parameters
                    .into_iter()
                    .map(|param| Annotation {
                        location: param.location,
                        name: param.name,
                        value: self.specialize_type(param.value),
                    })
                    .collect::<Vec<_>>();

                Ok((
                    ASTNode::Lambda {
                        parameters: args,
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
                return_ty,
            } => {
                let (specialized_condition, mut n1) = self.specialize_expr(*condition)?;
                let (specialized_then, n2) = self.specialize_expr(*then_branch)?;
                let (specialized_else, n3) = self.specialize_expr(*else_branch)?;

                n1.extend(n2);
                n1.extend(n3);

                let return_ty = self.specialize_type(return_ty.clone());

                Ok((
                    ASTNode::If {
                        condition: Box::new(specialized_condition),
                        then_branch: Box::new(specialized_then),
                        else_branch: Box::new(specialized_else),
                        return_ty,
                    },
                    n1,
                ))
            }

            ASTNode::Identifier(name) => {
                let ty = self.specialize_type(name.value.clone());

                if let Some(scheme) = self.variables.get(&name.name) {
                    let (specialized, ns) = self.specialize_identifier(name, scheme.clone())?;

                    return Ok((ASTNode::Identifier(specialized), ns));
                }

                if let Some(_) = self
                    .implementations
                    .iter()
                    .find(|((impl_name, _), _)| impl_name == &name.name)
                {
                    let (specialized, ns) = self.specialize_implementation(name)?;

                    return Ok((ASTNode::Identifier(specialized), ns));
                }

                return Ok((
                    ASTNode::Identifier(Annotation {
                        location: name.location,
                        name: name.name,
                        value: ty,
                    }),
                    vec![],
                ));
            }

            ASTNode::Application {
                function,
                arguments,
                function_type,
            } => {
                let (specialized_function, mut n1) = self.specialize_expr(*function)?;

                let (specialized_arguments, n2) = arguments
                    .into_iter()
                    .map(|arg| self.specialize_expr(arg))
                    .collect::<Result<(Vec<_>, Vec<_>)>>()?;

                let n2_concatenated = n2.into_iter().flatten().collect::<Vec<_>>();
                n1.extend(n2_concatenated);

                let function_type = self.specialize_type(function_type.clone());

                Ok((
                    ASTNode::Application {
                        function: Box::new(specialized_function),
                        arguments: specialized_arguments,
                        function_type,
                    },
                    n1,
                ))
            }
        }
    }

    fn specialize_implementation(
        &mut self,
        name: Annotation<Type>,
    ) -> Result<(Annotation<Type>, Vec<TypedToplevelNode>)> {
        let (prop_scheme, _) = if let Some(prop) = self.properties.get(&name.name) {
            prop.clone()
        } else {
            return Err(RealityError::UnknownStructure(name.name));
        };

        let (scheme_ty, _) = self.typechecker.instantiate_and_sub(prop_scheme.clone());

        self.typechecker
            .is_subtype(scheme_ty.clone(), name.value.clone())?;

        let impls = self.implementations.clone();

        let corresponding_implementation = impls.iter().find(|((impl_name, scheme), _)| {
            let ty = self.typechecker.instantiate(scheme.clone());

            return impl_name == &name.name
                && self
                    .typechecker
                    .is_subtype(ty.clone(), scheme_ty.clone())
                    .is_ok();
        });

        if let Some(((_, _), node)) = corresponding_implementation {
            let (impl_ty, subst) = self.typechecker.instantiate_and_sub(prop_scheme.clone());

            self.typechecker.is_subtype(impl_ty, name.value.clone())?;

            let ordered_vars = prop_scheme
                .variables
                .iter()
                .map(|var| {
                    return subst.get(var).ok_or(RealityError::UnboundGenerics(Generics(
                        prop_scheme.variables.clone(),
                    )));
                })
                .collect::<Result<Vec<_>>>()?;

            let new_name = format!(
                "{}_{}",
                name.name,
                ordered_vars
                    .iter()
                    .map(|&v| { self.remove_alias(v.clone()).to_string() })
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            if let ToplevelNode::FunctionDeclaration {
                parameters,
                return_type,
                body,
                ..
            } = node
                && !self.already_converted_implementations.contains(&new_name)
            {
                let subst_as_vec = subst
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>();

                let new_parameters = parameters
                    .into_iter()
                    .map(|param| Annotation {
                        name: param.name.clone(),
                        value: self.specialize_type(param.value.substitute_all(subst_as_vec.clone())),
                        location: param.location,
                    })
                    .collect::<Vec<_>>();

                let (new_body, mut ns) = self.specialize_expr(*body.clone())?;

                let new_return_type = self.specialize_type(return_type.substitute_all(subst_as_vec));

                self.already_converted_implementations
                    .push(new_name.clone());

                ns.push(ToplevelNode::FunctionDeclaration {
                    name: Annotation {
                        name: new_name.clone(),
                        value: vec![],
                        location: name.location,
                    },
                    parameters: new_parameters,
                    return_type: new_return_type,
                    body: Box::new(new_body),
                });

                return Ok((
                    Annotation {
                        name: new_name.clone(),
                        value: name.value,
                        location: name.location,
                    },
                    ns,
                ));
            }

            return Ok((
                Annotation {
                    name: new_name.clone(),
                    value: name.value,
                    location: name.location,
                },
                vec![],
            ));
        }

        Ok((
            Annotation {
                name: name.name,
                value: name.value,
                location: name.location,
            },
            vec![],
        ))
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

            let new_name = format!(
                "{}_{}",
                name.name,
                new_subst
                    .values()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join("_")
            );

            let (new_body, mut ns) = self.specialize_expr(new_body)?;

            if !self.already_converted_variables.contains(&new_name) {
                ns.push(ToplevelNode::FunctionDeclaration {
                    name: Annotation {
                        name: new_name.clone(),
                        value: vec![],
                        location: name.location,
                    },
                    parameters: new_parameters,
                    return_type: new_return_type,
                    body: Box::new(new_body),
                });
            }

            self.already_converted_variables.push(new_name.clone());

            return Ok((
                Annotation {
                    name: new_name.clone(),
                    value: ty,
                    location: name.location,
                },
                ns,
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
            ASTNode::StructureCreation {
                fields,
                structure_name:
                    Annotation {
                        location,
                        name,
                        value,
                    },
            } => {
                let specialized_fields = fields
                    .into_iter()
                    .map(|(name, field)| (name, self.apply_sub(field, subst)))
                    .collect();

                let structure_name = Annotation {
                    location,
                    name,
                    value: value.substitute_all(
                        subst
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .collect::<Vec<_>>(),
                    ),
                };

                ASTNode::StructureCreation {
                    fields: specialized_fields,
                    structure_name,
                }
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
                return_ty,
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
                    return_ty: return_ty.substitute_all(subst_as_vec.clone()),
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
                return_ty,
            } => {
                let specialized_condition = self.apply_sub(*condition, subst);
                let specialized_then = self.apply_sub(*then_branch, subst);
                let specialized_else = self.apply_sub(*else_branch, subst);

                let subst_as_vec = subst
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>();

                let specialized_return_ty = return_ty.substitute_all(subst_as_vec);

                ASTNode::If {
                    condition: Box::new(specialized_condition),
                    then_branch: Box::new(specialized_then),
                    else_branch: Box::new(specialized_else),
                    return_ty: specialized_return_ty,
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
                function_type,
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

    fn specialize_type(&mut self, ty: Type) -> Type {
        match ty.clone() {
            Type::TypeIdentifier(name) => Type::TypeIdentifier(name),
            Type::TypeVariable(name) => Type::TypeVariable(name),
            Type::TypeFunction {
                parameters,
                return_type,
            } => Type::TypeFunction {
                parameters: parameters
                    .into_iter()
                    .map(|param| self.specialize_type(param))
                    .collect(),
                return_type: Box::new(self.specialize_type(*return_type)),
            },

            Type::TypeApplication(name, args) => {
                if let Type::TypeIdentifier(name) = self.remove_alias(*name.clone())
                    && let Ok((_, struct_type, _)) = self.specialize_structure(ty, name)
                {
                    // If specialization is successful, use the new structure
                    return struct_type;
                    // Continue with the new structure
                }

                let specialized_args = args
                    .into_iter()
                    .map(|arg| self.specialize_type(arg))
                    .collect();

                let specialized_function_type = self.specialize_type(*name);

                Type::TypeApplication(Box::new(specialized_function_type), specialized_args)
            }
            Type::TypeAliased(inner, alias) => {
                Type::TypeAliased(Box::new(self.specialize_type(*inner)), alias)
            }
            Type::AnonymousStructure { fields } => Type::AnonymousStructure {
                fields: fields
                    .into_iter()
                    .map(|(name, ty)| (name, self.specialize_type(ty)))
                    .collect(),
            },
        }
    }

    fn remove_alias(&self, ty: Type) -> Type {
        match ty.clone() {
            Type::TypeAliased(inner, _) => self.remove_alias(*inner),
            types::Type::TypeVariable(var) => {
                if let TypeVariable::Bound(ty) = &*var.borrow() {
                    self.remove_alias(*ty.clone())
                } else {
                    ty
                }
            }
            _ => ty,
        }
    }
}
