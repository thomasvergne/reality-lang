use std::{cell::RefCell, collections::HashMap, rc::Rc};

use reality_ast::{
    ASTNode, ToplevelNode, TypedASTNode, TypedToplevelNode,
    internal::{
        annotation::Annotation,
        literal::Literal,
        types::{Type, TypeVariable},
    },
};
use reality_error::RealityError;

pub mod hoisting;

pub struct ClosureConverter {
    pub source: (usize, usize, String),

    pub symbol_counter: Rc<RefCell<usize>>,

    pub globals: HashMap<String, (usize, Type<String>)>,
    pub natives: HashMap<String, (usize, Type<String>)>,
    pub locals: HashMap<String, Type<String>>,
    pub structures: HashMap<String, HashMap<String, Type<String>>>,
}

type Result<T> = std::result::Result<(T, Vec<TypedToplevelNode>, Type<String>), RealityError>;

impl ClosureConverter {
    pub fn new() -> Self {
        ClosureConverter {
            source: (0, 0, String::new()),
            symbol_counter: Rc::new(RefCell::new(0)),
            globals: HashMap::new(),
            locals: HashMap::new(),
            natives: HashMap::new(),
            structures: HashMap::new(),
        }
    }

    fn void_pointer(&mut self) -> Type<String> {
        Type::TypeApplication(
            Box::new(Type::TypeIdentifier("pointer".to_string())),
            vec![Type::TypeIdentifier("void".to_string())],
        )
    }

    fn is_void_pointer(&mut self, ty: &Type<String>) -> bool {
        match ty {
            Type::TypeApplication(func, args) => {
                *func.clone() == self.void_pointer() && args.is_empty()
            }
            _ => false,
        }
    }

    fn convert_type(&mut self, ty: Type<String>) -> Type<String> {
        match ty.clone() {
            Type::TypeIdentifier(name) => Type::TypeIdentifier(name),

            Type::TypeFunction {
                parameters,
                return_type,
            } if !self.is_void_pointer(&*return_type) => {
                let new_params = parameters
                    .into_iter()
                    .map(|p| self.convert_type(p))
                    .collect::<Vec<_>>();

                let new_return = self.convert_type(*return_type);

                let env_var = self.void_pointer();

                Type::AnonymousStructure {
                    fields: HashMap::from([
                        ("env".to_string(), env_var.clone()),
                        (
                            "fun".to_string(),
                            Type::TypeFunction {
                                parameters: vec![env_var]
                                    .iter()
                                    .chain(new_params.iter())
                                    .cloned()
                                    .collect(),
                                return_type: new_return.into(),
                            },
                        ),
                    ]),
                }
            }
            
            Type::TypeFunction { parameters, return_type } => {
                let new_params = parameters
                    .into_iter()
                    .map(|p| self.convert_type(p))
                    .collect::<Vec<_>>();

                let new_return = self.convert_type(*return_type);

        
                return Type::TypeFunction {
                    parameters: new_params,
                    return_type: new_return.into(),
                };
            }

            Type::TypeVariable(var) => {
                if let TypeVariable::Bound(ty) = &*var.borrow() {
                    return self.convert_type(*ty.clone());
                }

                ty
            }

            Type::TypeApplication(func, args) => {
                let new_func = self.convert_type(*func);
                let new_args = args.into_iter().map(|a| self.convert_type(a)).collect();

                Type::TypeApplication(Box::new(new_func), new_args)
            }

            Type::TypeAliased(ty, _) => {
                let new_ty = self.convert_type(*ty);
                new_ty
            }

            Type::AnonymousStructure { fields } => {
                let fields = fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.convert_type(v.clone())))
                    .collect();
                Type::AnonymousStructure { fields }
            }
        }
    }

    pub fn convert(
        &mut self,
        ast: Vec<TypedToplevelNode>,
    ) -> std::result::Result<Vec<TypedToplevelNode>, RealityError> {
        let mut new_ast = Vec::new();

        for node in ast {
            let (converted, ns, _) = self.convert_node(node)?;
            new_ast.extend(ns);
            new_ast.push(converted);
        }

        Ok(new_ast)
    }

    fn convert_node(&mut self, node: TypedToplevelNode) -> Result<TypedToplevelNode> {
        match node {
            ToplevelNode::ExternalFunction { name, parameters, return_type } => {
                let converted_params = parameters
                    .iter()
                    .map(|p| Annotation {
                        name: p.name.clone(),
                        value: self.convert_type(p.value.clone()),
                        location: p.location,
                    })
                    .collect::<Vec<_>>();

                let converted_return_type = self.convert_type(return_type);

                self.natives.insert(
                    name.name.clone(),
                    (
                        converted_params.len(),
                        Type::TypeFunction {
                            parameters: converted_params.iter().map(|p| p.value.clone()).collect(),
                            return_type: Box::new(converted_return_type.clone()),
                        },
                    ),
                );

                Ok((
                    ToplevelNode::ExternalFunction {
                        name,
                        parameters: converted_params,
                        return_type: converted_return_type,
                    },
                    Vec::new(),
                    self.void_pointer(),
                ))
            }

            ToplevelNode::FunctionDeclaration {
                name,
                parameters,
                body,
                return_type,
            } => {
                let converted_params = parameters
                    .iter()
                    .map(|p| self.convert_type(p.value.clone()))
                    .collect::<Vec<_>>();

                let converted_return_type = self.convert_type(return_type);

                self.globals.insert(
                    name.clone().name,
                    (
                        converted_params.len(),
                        Type::TypeFunction {
                            parameters: converted_params,
                            return_type: Box::new(converted_return_type),
                        },
                    ),
                );

                let (converted_body, ns, ty) = self.convert_exp(*body)?;

                let param_types = parameters
                    .iter()
                    .map(|p| Annotation {
                        name: p.name.clone(),
                        location: p.location,
                        value: self.convert_type(p.value.clone()),
                    })
                    .collect::<Vec<_>>();

                self.globals.remove(&name.clone().name);
                self.globals.insert(
                    name.clone().name,
                    (
                        param_types.len(),
                        Type::TypeFunction {
                            parameters: param_types.iter().map(|p| p.value.clone()).collect(),
                            return_type: Box::new(ty.clone()),
                        },
                    ),
                );

                Ok((
                    ToplevelNode::FunctionDeclaration {
                        name,
                        parameters,
                        return_type: ty.clone(),
                        body: Box::new(converted_body),
                    },
                    ns,
                    ty,
                ))
            }
            ToplevelNode::StructureDeclaration { header, fields } => {
                let mut hm = HashMap::new();

                for (name, value) in fields {
                    let converted_type = self.convert_type(value);
                    hm.insert(name, converted_type);
                }

                self.structures.insert(header.name.clone(), hm.clone());

                Ok((
                    ToplevelNode::StructureDeclaration { header, fields: hm },
                    vec![],
                    self.void_pointer(),
                ))
            }

            ToplevelNode::Located { span, node } => {
                let (converted, ns, ty) = self.convert_node(*node)?;
                Ok((ToplevelNode::Located { span, node: Box::new(converted) }, ns, ty))
            }

            _ => Ok((node, Vec::new(), self.void_pointer())),
        }
    }

    fn convert_exp(&mut self, expr: TypedASTNode) -> Result<TypedASTNode> {
        // Implement the conversion logic for a single expression here
        match expr {
            ASTNode::Lambda { .. } => self.convert_lambda(expr, vec![]),

            ASTNode::LetIn {
                variable,
                value,
                body,
                ..
            } => match value.get_lambda() {
                Some(lambda) => {
                    self.locals
                        .insert(variable.name.clone(), variable.value.clone());

                    let (converted, mut ns, lambda_ty) =
                        self.convert_lambda(lambda.clone(), vec![variable.name.clone()])?;

                    self.locals.remove(&variable.name);
                    self.locals.insert(variable.name.clone(), lambda_ty.clone());

                    let (converted_body, mut ns2, body_ty) = self.convert_exp(*body)?;

                    self.locals.remove(&variable.name);

                    ns.append(&mut ns2);

                    Ok((
                        ASTNode::LetIn {
                            variable: Annotation {
                                name: variable.name.clone(),
                                location: variable.location,
                                value: lambda_ty,
                            },
                            value: Box::new(converted),
                            body: Box::new(converted_body),
                            return_ty: body_ty.clone(),
                        },
                        ns,
                        body_ty,
                    ))
                }

                None => {
                    let (converted_value, mut ns1, value_ty) = self.convert_exp(*value)?;
                    let (converted_body, mut ns2, body_ty) = self.convert_exp(*body)?;
                    ns1.append(&mut ns2);

                    Ok((
                        ASTNode::LetIn {
                            variable: Annotation {
                                name: variable.name.clone(),
                                location: variable.location,
                                value: value_ty,
                            },
                            value: Box::new(converted_value),
                            body: Box::new(converted_body),
                            return_ty: body_ty.clone(),
                        },
                        ns1,
                        body_ty,
                    ))
                }
            },

            ASTNode::StructureCreation {
                structure_name,
                fields,
            } => {
                let mut specialized_fields = HashMap::new();
                let mut ns = Vec::new();

                for (name, value) in fields {
                    let (converted_value, mut ns1, _) = self.convert_exp(value)?;

                    specialized_fields.insert(name, converted_value);

                    ns.append(&mut ns1);
                }

                Ok((
                    ASTNode::StructureCreation {
                        structure_name: structure_name.clone(),
                        fields: specialized_fields,
                    },
                    ns,
                    Type::TypeIdentifier(structure_name.name),
                ))
            }

            ASTNode::StructureAccess { structure, field } => {
                let (converted_structure, ns, ty) = self.convert_exp(*structure)?;

                let mut field_ty = self.void_pointer();

                if let Type::TypeIdentifier(name) = ty {
                    if let Some(fields) = self.structures.get(&name) {
                        if let Some(ft) = fields.get(&field) {
                            field_ty = ft.clone();
                        }
                    }
                }

                Ok((
                    ASTNode::StructureAccess {
                        structure: Box::new(converted_structure),
                        field,
                    },
                    ns,
                    field_ty,
                ))
            }

            ASTNode::Located { span, node } => {
                let (converted_node, ns, ty) = self.convert_exp(*node)?;
                Ok((
                    ASTNode::Located {
                        span,
                        node: Box::new(converted_node),
                    },
                    ns,
                    ty,
                ))
            }

            ASTNode::Literal(value) => Ok((
                ASTNode::Literal(value.clone()),
                Vec::new(),
                match value {
                    Literal::Integer(_) => Type::TypeIdentifier("i32".to_string()),
                    Literal::Boolean(_) => Type::TypeIdentifier("bool".to_string()),
                    Literal::String(_) => Type::TypeApplication(
                        Box::new(Type::TypeIdentifier("pointer".to_string())),
                        vec![Type::TypeIdentifier("u8".to_string())],
                    ),
                    Literal::Character(_) => Type::TypeIdentifier("char".to_string()),
                    Literal::Float(_) => Type::TypeIdentifier("f32".to_string()),
                },
            )),

            ASTNode::If {
                condition,
                then_branch,
                else_branch,

                return_ty,
            } => {
                let (converted_condition, mut ns1, _) = self.convert_exp(*condition)?;
                let (converted_then, mut ns2, then_ty) = self.convert_exp(*then_branch)?;
                let (converted_else, mut ns3, else_ty) = self.convert_exp(*else_branch)?;
                let converted_return_ty = self.convert_type(return_ty);

                ns1.append(&mut ns2);
                ns1.append(&mut ns3);

                let return_ty = if then_ty == else_ty {
                    then_ty
                } else {
                    self.void_pointer()
                };

                Ok((
                    ASTNode::If {
                        condition: Box::new(converted_condition),
                        then_branch: Box::new(converted_then),
                        else_branch: Box::new(converted_else),
                        return_ty: converted_return_ty,
                    },
                    ns1,
                    return_ty
                ))
            }

            ASTNode::Identifier(name) => {
                let mut converted_type = self.convert_type(name.value.clone());

                if let Some(ty) = self.locals.get(&name.name) {
                    converted_type = ty.clone();
                } else if let Some((_, ty)) = self.natives.get(&name.name) {
                    converted_type = ty.clone();
                } else if let Some((_, ty)) = self.globals.get(&name.name) {
                    converted_type = ty.clone();
                }

                Ok((
                    ASTNode::Identifier(Annotation {
                        name: name.name,
                        location: name.location,
                        value: converted_type.clone(),
                    }),
                    vec![],
                    converted_type,
                ))
            }

            ASTNode::Application {
                function,
                arguments,
                function_type,
            } => {
                let mut new_arguments = vec![];
                let mut ns = vec![];

                let mut native_functions = self.natives.clone();
                let global_functions = self.globals.clone();

                native_functions.extend(global_functions);

                for argument in arguments {
                    let (converted_argument, mut ns1, _) = self.convert_exp(argument)?;
                    new_arguments.push(converted_argument);
                    ns.append(&mut ns1);
                }

                if let ASTNode::Identifier(ann) = function.flatten_locations()
                    && let Some((_, ty)) = native_functions.get(&ann.name)
                {
                    let return_ty = if let Type::TypeFunction { return_type, .. } = ty {
                        self.convert_type(*return_type.clone())
                    } else {
                        Type::TypeIdentifier("unit".to_string())
                    };

                    return Ok((
                        ASTNode::Application {
                            function,
                            arguments: new_arguments,
                            function_type: function_type.clone(),
                        },
                        ns,
                        return_ty
                    ));
                }

                let (converted_function, mut ns1, fun_ty) = self.convert_exp(*function)?;

                ns.append(&mut ns1);

                let lambda_name = format!("closure_lambda_{}", self.symbol_counter.borrow());
                self.symbol_counter.replace_with(|v| *v + 1);

                let call_var = ASTNode::Identifier(Annotation {
                    name: lambda_name.clone(),
                    value: Type::TypeIdentifier(lambda_name.clone()),
                    location: (0, 0),
                });

                let function = ASTNode::StructureAccess {
                    structure: Box::new(call_var.clone()),
                    field: "fun".to_string(),
                };

                let env = ASTNode::StructureAccess {
                    structure: Box::new(call_var.clone()),
                    field: "env".to_string(),
                };

                let lambda_ty = self.convert_type(function_type.clone());

                let call = ASTNode::Application {
                    function: Box::new(function.clone()),
                    arguments: vec![env]
                        .iter()
                        .chain(new_arguments.iter())
                        .cloned()
                        .collect(),
                    function_type: lambda_ty.clone(),
                };

                let converted_return_ty;

                if let Type::TypeFunction { return_type, .. } = function_type {
                    converted_return_ty = self.convert_type(*return_type);
                } else {
                    converted_return_ty = Type::TypeIdentifier("unit".to_string());
                }

                Ok((
                    ASTNode::LetIn {
                        variable: Annotation {
                            name: lambda_name.clone(),
                            location: (0, 0),
                            value: fun_ty,
                        },
                        body: Box::new(call),
                        value: Box::new(converted_function),
                        return_ty: converted_return_ty.clone(),
                    },
                    ns,
                    converted_return_ty
                ))
            }
        }
    }

    fn convert_lambda(
        &mut self,
        expr: TypedASTNode,
        reserved: Vec<String>,
    ) -> Result<TypedASTNode> {
        if let ASTNode::Lambda {
            parameters,
            body,
            ..
        } = expr.clone()
        {
            let mut free_vars = body.free_variables();
            let native_functions = self.natives.clone();
            let global_functions = self.globals.clone();

            let final_native_functions = native_functions.keys().collect::<Vec<_>>();
            let final_globals_functions = global_functions.keys().collect::<Vec<_>>();

            let grouped_functions = final_native_functions
                .iter()
                .chain(final_globals_functions.iter())
                .cloned()
                .collect::<Vec<_>>();

            let parameter_names = parameters
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<_>>();

            let final_functions = grouped_functions
                .into_iter()
                .filter(|f| !parameter_names.contains(f))
                .collect::<Vec<_>>();

            // freeVars Set.\\ (finalNativesFuns <> Set.fromList args <> reserved)
            free_vars.retain(|v, _| {
                !final_functions.contains(&v)
                    && !parameter_names.contains(&v)
                    && !reserved.contains(&v)
            });

            let structure_name = format!("closure_struct_{}", self.symbol_counter.borrow());
            let lambda_struct_name = format!("closure_lambda_{}", self.symbol_counter.borrow());

            let structure = ASTNode::StructureCreation {
                structure_name: Annotation {
                    name: structure_name.clone(),
                    location: (0, 0),
                    value: Type::TypeIdentifier(structure_name.clone()),
                },
                fields: free_vars
                    .iter()
                    .map(|(v, t)| {
                        (
                            v.clone(),
                            ASTNode::Identifier(Annotation {
                                name: v.clone(),
                                value: t.clone(),
                                location: (0, 0),
                            }),
                        )
                    })
                    .collect(),
            };

            let env_var = ASTNode::Identifier(Annotation {
                name: format!("closure_env_{}", self.symbol_counter.borrow()),
                value: Type::TypeIdentifier(structure_name.clone()),
                location: (0, 0),
            });

            let old_locals = self.locals.clone();
            self.locals.extend(
                parameters
                    .iter()
                    .map(|ann| (ann.name.clone(), ann.value.clone())),
            );

            let (body, mut ns, body_ty) = self.convert_exp(*body.clone())?;

            self.locals = old_locals;

            let new_body = free_vars.iter().fold(body, |acc, (v, t)| ASTNode::LetIn {
                variable: Annotation {
                    name: v.clone(),
                    value: t.clone(),
                    location: (0, 0),
                },
                value: Box::new(ASTNode::StructureAccess {
                    structure: Box::new(env_var.clone()),
                    field: v.clone(),
                }),
                body: Box::new(acc),
                return_ty: body_ty.clone(),
            });

            let structure_definition: ToplevelNode<String, Type<String>, Type<String>> =
                ToplevelNode::StructureDeclaration {
                    header: Annotation {
                        name: structure_name.clone(),
                        value: vec![],
                        location: (0, 0),
                    },
                    fields: free_vars,
                };

            let mut param_tys = vec![];

            param_tys.push(Type::TypeIdentifier(structure_name.clone()));

            for p in parameters.clone() {
                param_tys.push(self.convert_type(p.value));
            }

            let lambda_structure_definition: ToplevelNode<String, Type<String>, Type<String>> =
                ToplevelNode::StructureDeclaration {
                    header: Annotation {
                        name: lambda_struct_name.clone(),
                        value: vec![],
                        location: (0, 0),
                    },
                    fields: HashMap::from([
                        (
                            "env".to_string(),
                            Type::TypeIdentifier(structure_name.clone()),
                        ),
                        (
                            "fun".to_string(),
                            Type::TypeFunction {
                                parameters: param_tys,
                                return_type: Box::new(body_ty.clone()),
                            },
                        ),
                    ]),
                };

            let env_annot = Annotation {
                name: format!("closure_env_{}", self.symbol_counter.borrow()),
                value: Type::TypeIdentifier(structure_name.clone()),
                location: (0, 0),
            };

            let lambda_structure = ASTNode::StructureCreation {
                structure_name: Annotation {
                    name: lambda_struct_name.clone(),
                    location: (0, 0),
                    value: Type::TypeIdentifier(lambda_struct_name.clone()),
                },
                fields: HashMap::from([
                    ("env".to_string(), structure),
                    (
                        "fun".to_string(),
                        ASTNode::Lambda {
                            parameters: vec![env_annot]
                                .iter()
                                .chain(parameters.iter())
                                .map(|ann| ann.clone())
                                .collect::<Vec<_>>(),
                            return_type: body_ty,
                            body: Box::new(new_body),
                        },
                    ),
                ]),
            };

            ns.push(structure_definition);
            ns.push(lambda_structure_definition);

            self.symbol_counter.replace_with(|v| *v + 1);

            return Ok((lambda_structure, ns, Type::TypeIdentifier(lambda_struct_name)));
        }

        Ok((expr, vec![], self.void_pointer()))
    }
}
