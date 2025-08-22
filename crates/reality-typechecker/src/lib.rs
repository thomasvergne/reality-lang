use std::{cell::RefCell, collections::HashMap};

use reality_ast::{
    ASTNode, ToplevelNode,
    internal::{
        annotation::Annotation,
        literal::Literal,
        types::{self, TypeVariable},
    },
};
use reality_error::{Generics, RealityError};

pub struct Typechecker<'a> {
    pub input: &'a str,
    pub file: &'a str,

    pub position: (usize, usize),

    counter: RefCell<usize>,
    environment: HashMap<String, Scheme>,
    level: RefCell<usize>,
    type_aliases: HashMap<String, Scheme>,
}

type Type = types::Type<String>;
type Scheme = types::Scheme<String>;
type TypedASTNode = ASTNode<String, Type>;
type TypedToplevelNode = ToplevelNode<String, Type, Type>;

type Result<T> = std::result::Result<T, reality_error::RealityError>;

impl<'a> Typechecker<'a> {
    pub fn new(input: &'a str, file: &'a str) -> Self {
        Typechecker {
            input,
            file,
            position: (0, 0),
            counter: RefCell::new(0),
            environment: HashMap::new(),
            level: RefCell::new(0),
            type_aliases: HashMap::new(),
        }
    }

    fn new_symbol(&mut self, prefix: String) -> String {
        let mut counter = self.counter.borrow_mut();
        *counter += 1;
        format!("${}{}", prefix, counter)
    }

    fn new_type(&mut self, prefix: String) -> Type {
        use std::rc::Rc;
        types::Type::TypeVariable(Rc::new(std::cell::RefCell::new(
            types::TypeVariable::Unbound(self.new_symbol(prefix), *self.level.borrow()),
        )))
    }

    pub fn check_program(&mut self, ast: Vec<ToplevelNode>) -> Result<Vec<TypedToplevelNode>> {
        let mut checked_nodes = Vec::new();

        for node in ast {
            let checked_node = self.check_toplevel(node)?;
            checked_nodes.push(checked_node);
        }

        Ok(checked_nodes)
    }

    pub fn check_toplevel(&mut self, node: ToplevelNode) -> Result<TypedToplevelNode> {
        match node {
            ToplevelNode::Located { span, node } => {
                let old_position = self.position;
                let old_file_name = self.file;
                self.position = (span.0, span.1);
                self.file = Box::leak(span.2.clone().into_boxed_str());

                match self.check_toplevel(*node) {
                    Ok(result) => {
                        self.position = old_position;
                        self.file = old_file_name;
                        Ok(result)
                    }
                    Err(err) => Err(err),
                }
            }

            ToplevelNode::FunctionDeclaration {
                name,
                parameters,
                body,
                return_type,
            } => {
                let old_env = self.environment.clone();
                let mut new_params = Vec::new();
                for param in parameters {
                    let ty = param.value.normalize();
                    new_params.push(Annotation {
                        name: param.name.clone(),
                        value: ty.clone(),
                        location: param.location,
                    });

                    self.environment.insert(
                        param.name.clone(),
                        Scheme {
                            variables: vec![],
                            body: ty.clone(),
                        },
                    );
                }

                let return_type = return_type.normalize();

                let scheme = Scheme {
                    variables: name.clone().value,
                    body: Type::TypeFunction {
                        parameters: new_params.iter().map(|p| p.value.clone()).collect(),
                        return_type: Box::new(return_type.clone()),
                    },
                };

                self.environment.insert(name.name.clone(), scheme.clone());

                let body = self.check(*body, return_type.clone())?;

                self.environment.clear();
                self.environment.extend(old_env);
                self.environment.insert(name.name.clone(), scheme);

                let free_types = body.free_types();

                if free_types.len() > 0 {
                    let portion = free_types[0..3].to_vec();

                    return Err(RealityError::UnboundGenerics(Generics(portion)));
                }

                Ok(ToplevelNode::FunctionDeclaration {
                    name,
                    parameters: new_params,
                    body: Box::new(body),
                    return_type,
                })
            }

            ToplevelNode::ConstantDeclaration { variable, value } => {
                let final_ty: Type;

                let (value, ty) = self.synthesize(*value)?;

                if let Some(original_ty) = variable.value {
                    self.is_subtype(ty.clone(), original_ty.normalize())?;

                    final_ty = original_ty.normalize();
                } else {
                    final_ty = ty.clone();
                }

                self.environment.insert(
                    variable.name.clone(),
                    Scheme {
                        variables: vec![],
                        body: final_ty.clone(),
                    },
                );
                Ok(ToplevelNode::ConstantDeclaration {
                    variable: Annotation {
                        name: variable.name,
                        value: final_ty,
                        location: variable.location,
                    },
                    value: Box::new(value),
                })
            }

            ToplevelNode::TypeAlias { name, body } => {
                let body = body.normalize();
                self.type_aliases.insert(
                    name.name.clone(),
                    Scheme {
                        variables: vec![],
                        body: body.clone(),
                    },
                );

                Ok(ToplevelNode::TypeAlias { name, body })
            }

            ToplevelNode::PublicDeclaration(inner) => self.check_toplevel(*inner),

            ToplevelNode::ImportDeclaration(_) => {
                Err(reality_error::RealityError::NoRequireStatement)
            }

            ToplevelNode::ModuleDeclaration { .. } => {
                Err(reality_error::RealityError::NoModuleDeclaration)
            }
        }
    }

    fn synthesize(
        &mut self,
        expr: ASTNode<Vec<String>, Option<types::Type<Vec<String>>>>,
    ) -> Result<(TypedASTNode, Type)> {
        match expr {
            ASTNode::Located { span, node } => {
                let old_position = self.position;
                let old_file_name = self.file;
                self.position = (span.0, span.1);
                self.file = Box::leak(span.2.clone().into_boxed_str());

                let (checked_node, ty) = self.synthesize(*node)?;

                self.position = old_position;
                self.file = old_file_name;
                Ok((checked_node, ty))
            }

            ASTNode::Literal(value) => {
                let ty = match value {
                    Literal::Integer(_) => Type::TypeIdentifier("i32".to_string()),
                    Literal::String(_) => Type::TypeIdentifier("string".to_string()),
                    Literal::Boolean(_) => Type::TypeIdentifier("bool".to_string()),
                    Literal::Float(_) => Type::TypeIdentifier("f32".to_string()),
                    Literal::Character(_) => Type::TypeIdentifier("char".to_string()),
                };

                Ok((ASTNode::Literal(value), ty))
            }

            ASTNode::LetIn {
                variable,
                value,
                body,
            } => {
                let annot_type: Type;
                let typed_value: TypedASTNode;

                if let Some(ty) = variable.value {
                    annot_type = ty.normalize();
                    typed_value = self.check(*value, annot_type.clone())?;
                } else {
                    let (value, ty) = self.synthesize(*value)?;
                    annot_type = ty;
                    typed_value = value;
                }

                self.environment.insert(
                    variable.name.clone(),
                    Scheme {
                        variables: vec![],
                        body: annot_type.clone(),
                    },
                );

                let (body, return_type) = self.synthesize(*body)?;

                self.environment.remove(&variable.name);

                return Ok((
                    ASTNode::LetIn {
                        variable: Annotation {
                            name: variable.name,
                            value: annot_type,
                            location: variable.location,
                        },
                        value: Box::new(typed_value),
                        body: Box::new(body),
                    },
                    return_type,
                ));
            }

            ASTNode::Lambda {
                parameters,
                return_type,
                body,
            } => {
                let mut new_parameters: Vec<Annotation<Type, String>> = Vec::new();

                for param in parameters {
                    let param_ty = if let Some(ty) = param.value {
                        ty.normalize()
                    } else {
                        self.new_type("param".to_string())
                    };
                    new_parameters.push(Annotation {
                        name: param.name,
                        value: param_ty,
                        location: param.location,
                    });
                }

                let old_env = self.environment.clone();
                self.environment.extend(new_parameters.iter().map(|p| {
                    (
                        p.name.clone(),
                        Scheme {
                            variables: vec![],
                            body: p.value.clone(),
                        },
                    )
                }));

                let (body, return_type) = if let Some(ty) = return_type {
                    let body = self.check(*body, ty.normalize())?;

                    (body, ty.normalize())
                } else {
                    let (body, ty) = self.synthesize(*body)?;

                    (body, ty)
                };

                self.environment = old_env;

                return Ok((
                    ASTNode::Lambda {
                        parameters: new_parameters.clone(),
                        return_type: return_type.clone(),
                        body: Box::new(body),
                    },
                    Type::TypeFunction {
                        parameters: new_parameters.iter().map(|p| p.value.clone()).collect(),
                        return_type: Box::new(return_type.clone()),
                    },
                ));
            }

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (condition, cond_ty) = self.synthesize(*condition)?;
                self.is_subtype(cond_ty, Type::TypeIdentifier("bool".to_string()))?;

                let (then_branch, then_ty) = self.synthesize(*then_branch)?;
                let else_branch = self.check(*else_branch, then_ty.clone())?;

                return Ok((
                    ASTNode::If {
                        condition: Box::new(condition),
                        then_branch: Box::new(then_branch),
                        else_branch: Box::new(else_branch),
                    },
                    then_ty,
                ));
            }

            ASTNode::Identifier(name) => {
                let new_name = name.name.join("::").to_string();
                if let Some(sch) = self.environment.get(&new_name) {
                    let ty = self.instantiate(sch.clone());

                    return Ok((
                        ASTNode::Identifier(Annotation {
                            name: new_name,
                            value: ty.clone(),
                            location: name.location,
                        }),
                        ty,
                    ));
                } else {
                    Err(reality_error::RealityError::VariableNotFound(new_name))
                }
            }

            ASTNode::Application {
                function,
                arguments,
            } => {
                let (head, func_ty) = self.synthesize((*function).clone())?;

                if let Type::TypeFunction {
                    return_type,
                    parameters,
                } = func_ty
                {
                    let args = arguments
                        .into_iter()
                        .zip(parameters)
                        .map(|(arg, ty)| self.check(arg, ty))
                        .collect::<Result<Vec<_>>>()?;

                    return Ok((
                        ASTNode::Application {
                            function: Box::new(head),
                            arguments: args,
                        },
                        *return_type,
                    ));
                }

                Err(reality_error::RealityError::ExpectedFunction(func_ty))
            }
        }
    }

    fn check(
        &mut self,
        expr: ASTNode<Vec<String>, Option<types::Type<Vec<String>>>>,
        expected: Type,
    ) -> Result<TypedASTNode> {
        match expr {
            ASTNode::Located { span, node } => {
                let old_position = self.position;
                let old_file_name = self.file;
                self.position = (span.0, span.1);
                self.file = Box::leak(span.2.clone().into_boxed_str());

                let checked_node = self.check(*node, expected);

                match checked_node {
                    Ok(node) => {
                        self.position = old_position;
                        self.file = old_file_name;

                        Ok(ASTNode::Located {
                            span,
                            node: Box::new(node),
                        })
                    }
                    Err(err) => Err(err),
                }
            }

            ASTNode::Literal(value) => self
                .check_literal(value, expected)
                .map(|v| ASTNode::Literal(v)),

            ASTNode::Identifier(name) => {
                let new_name = name.name.join("::").to_string();
                if let Some(sch) = self.environment.get(&new_name) {
                    let ty = self.instantiate(sch.clone());

                    self.is_subtype(ty.clone(), expected)?;

                    Ok(ASTNode::Identifier(Annotation {
                        name: new_name,
                        value: ty,
                        location: name.location,
                    }))
                } else {
                    Err(reality_error::RealityError::VariableNotFound(new_name))
                }
            }

            ASTNode::LetIn {
                variable,
                value,
                body,
            } => {
                let annot_type: Type;
                let typed_value: TypedASTNode;

                if let Some(ty) = variable.value {
                    annot_type = ty.normalize();
                    typed_value = self.check(*value, annot_type.clone())?;
                } else {
                    let (value, ty) = self.synthesize(*value)?;

                    annot_type = ty;
                    typed_value = value;
                }

                self.environment.insert(
                    variable.name.clone(),
                    Scheme {
                        variables: vec![],
                        body: annot_type.clone(),
                    },
                );

                let body = self.check(*body, expected)?;

                self.environment.remove(&variable.name);

                return Ok(ASTNode::LetIn {
                    variable: Annotation {
                        name: variable.name,
                        value: annot_type,
                        location: variable.location,
                    },
                    value: Box::new(typed_value),
                    body: Box::new(body),
                });
            }

            ASTNode::Lambda {
                parameters, body, return_type
            } => match expected {
                Type::TypeFunction {
                    parameters: param_tys,
                    return_type: ret_ty,
                } => {
                    let mut new_parameters: Vec<Annotation<Type, String>> = Vec::new();

                    for (param, param_ty) in parameters.iter().zip(param_tys) {
                        if let Some(ty) = &param.value {
                            self.is_subtype(ty.normalize(), param_ty.clone())?;
                        }

                        new_parameters.push(Annotation {
                            name: param.clone().name,
                            value: param_ty,
                            location: param.location,
                        });
                    }

                    let old_env = self.environment.clone();
                    self.environment.extend(new_parameters.iter().map(|p| {
                        (
                            p.name.clone(),
                            Scheme {
                                variables: vec![],
                                body: p.value.clone(),
                            },
                        )
                    }));

                    let mut ret  = *ret_ty.clone();

                    if let Some(return_type) = return_type {
                        self.is_subtype(return_type.normalize(), *ret_ty)?;

                        ret = return_type.normalize();
                    }

                    let body = self.check(*body, ret.clone())?;

                    self.environment = old_env;

                    Ok(ASTNode::Lambda {
                        parameters: new_parameters,
                        return_type: ret,
                        body: Box::new(body),
                    })
                }

                _ => Err(RealityError::ExpectedFunction(expected)),
            },

            ASTNode::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.check(*condition, Type::TypeIdentifier("bool".to_string()))?;
                let then_branch = self.check(*then_branch, expected.clone())?;
                let else_branch = self.check(*else_branch, expected.clone())?;

                Ok(ASTNode::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch: Box::new(else_branch),
                })
            }

            _ => {
                let (inferred, inferred_type) = self.synthesize(expr)?;

                println!("Inferred type: {:?}", inferred_type);

                self.is_subtype(inferred_type, expected)?;

                Ok(inferred)
            }
        }
    }

    pub fn instantiate(&mut self, scheme: Scheme) -> Type {
        let mut type_vars = HashMap::new();
        for var in scheme.variables {
            type_vars.insert(var.clone(), self.new_type(var.clone()));
        }

        type_vars
            .into_iter()
            .fold(scheme.body, |acc, (var, ty)| acc.substitute(&var, ty))
    }

    pub fn instantiate_and_sub(&mut self, scheme: Scheme) -> (Type, HashMap<String, Type>) {
        let mut type_vars = HashMap::new();
        for var in scheme.variables {
            type_vars.insert(var.clone(), self.new_type(var.clone()));
        }

        (
            type_vars
                .clone()
                .into_iter()
                .fold(scheme.body, |acc, (var, ty)| acc.substitute(&var, ty)),
            type_vars,
        )
    }

    fn check_literal(&mut self, value: Literal, expected: Type) -> Result<Literal> {
        match value {
            Literal::Integer(n) => {
                self.is_subtype(Type::TypeIdentifier("i32".to_string()), expected)?;

                Ok(Literal::Integer(n))
            }
            Literal::String(s) => {
                self.is_subtype(Type::TypeIdentifier("string".to_string()), expected)?;

                Ok(Literal::String(s))
            }

            Literal::Boolean(b) => {
                self.is_subtype(Type::TypeIdentifier("bool".to_string()), expected)?;

                Ok(Literal::Boolean(b))
            }

            Literal::Float(f) => {
                self.is_subtype(Type::TypeIdentifier("float".to_string()), expected)?;

                Ok(Literal::Float(f))
            }

            Literal::Character(c) => {
                self.is_subtype(Type::TypeIdentifier("char".to_string()), expected)?;

                Ok(Literal::Character(c))
            }
        }
    }

    pub fn is_subtype(&mut self, subtype: Type, supertype: Type) -> Result<()> {
        let old_subtype = subtype.clone();
        let old_supertype = supertype.clone();

        let subtype = self.remove_alias(subtype);
        let supertype = self.remove_alias(supertype);

        if subtype == supertype {
            return Ok(());
        }

        if let Some(size) = subtype.is_integer_type() {
            if let Some(super_size) = supertype.is_integer_type() {
                if size <= super_size {
                    return Ok(());
                } else {
                    return Err(reality_error::RealityError::TypeMismatch(
                        subtype.clone(),
                        supertype.clone(),
                    ));
                }
            }
        }

        if let Some(size) = subtype.is_unsigned_integer_type() {
            if let Some(super_size) = supertype.is_integer_type() {
                if size < super_size {
                    return Ok(());
                } else {
                    return Err(reality_error::RealityError::UnsignedIntegerMismatch(
                        subtype.clone(),
                        supertype.clone(),
                    ));
                }
            }
        }

        if let Some(_) = subtype.is_integer_type() {
            if let Some(_) = supertype.is_unsigned_integer_type() {
                return Err(reality_error::RealityError::UnsignedIntegerMismatch(
                    subtype.clone(),
                    supertype.clone(),
                ));
            }
        }

        if let Some(size) = subtype.is_unsigned_integer_type() {
            if let Some(super_size) = supertype.is_unsigned_integer_type() {
                if size <= super_size {
                    return Ok(());
                } else {
                    return Err(reality_error::RealityError::TypeMismatch(
                        subtype.clone(),
                        supertype.clone(),
                    ));
                }
            }
        }

        if let Some(size) = subtype.is_floating_point_type() {
            if let Some(super_size) = supertype.is_floating_point_type() {
                if size <= super_size {
                    return Ok(());
                } else {
                    return Err(reality_error::RealityError::TypeMismatch(
                        subtype.clone(),
                        supertype.clone(),
                    ));
                }
            }
        }

        return self.unifies_with(subtype, supertype, old_subtype, old_supertype);
    }

    fn remove_alias(&self, ty: Type) -> Type {
        match ty.clone() {
            Type::TypeIdentifier(name) if self.type_aliases.contains_key(&name) => {
                if let Some(scheme) = self.type_aliases.get(&name) {
                    if scheme.variables.is_empty() {
                        return Type::TypeAliased(Box::new(scheme.body.clone()), name);
                    }

                    return ty;
                }

                return ty;
            }

            Type::TypeApplication(base, args) => {
                let base = self.remove_alias(*base);
                let args = args
                    .iter()
                    .map(|arg| self.remove_alias(arg.clone()))
                    .collect::<Vec<_>>();

                if let Type::TypeIdentifier(name) = base {
                    match self.type_aliases.get(&name) {
                        Some(scheme) => {
                            if args.len() != scheme.variables.len() {
                                return ty;
                            }

                            let subst = scheme
                                .variables
                                .iter()
                                .cloned()
                                .zip(args)
                                .collect::<Vec<_>>();
                            return Type::TypeAliased(
                                Box::new(scheme.body.substitute_all(subst)),
                                name,
                            );
                        }
                        None => return ty,
                    }
                }

                return Type::TypeApplication(Box::new(base.clone()), args);
            }

            Type::TypeVariable(var) => {
                let mut_var = var.borrow_mut();
                match &*mut_var {
                    TypeVariable::Unbound(_, _) => return ty,
                    TypeVariable::Bound(ty) => self.remove_alias(*ty.clone()),
                }
            }

            Type::TypeFunction {
                parameters,
                return_type,
            } => {
                let parameters = parameters
                    .iter()
                    .map(|param| self.remove_alias(param.clone()))
                    .collect::<Vec<_>>();
                let return_type = self.remove_alias(*return_type).clone();

                return Type::TypeFunction {
                    parameters,
                    return_type: Box::new(return_type),
                };
            }

            _ => ty,
        }
    }

    fn does_occur_b(&self, tvr: &mut TypeVariable<String>, ty: &Type) -> Result<bool> {
        match ty {
            Type::TypeVariable(tv_) => {
                let mut mut_tv = tv_.borrow_mut();

                if let TypeVariable::Bound(ty) = &*mut_tv {
                    return self.does_occur_b(tvr, ty);
                } else if let TypeVariable::Unbound(name, lvl) = &*mut_tv {
                    if *tvr == *mut_tv {
                        return Ok(true);
                    }
                    // Update the level of the unbound variable
                    let new_min_lvl = match tvr {
                        TypeVariable::Bound(_) => *lvl,
                        TypeVariable::Unbound(_, lvl_) => std::cmp::min(*lvl_, *lvl),
                    };

                    *mut_tv = TypeVariable::Unbound(name.clone(), new_min_lvl);
                    return Ok(false);
                }

                Ok(false)
            }
            Type::TypeApplication(base, args) => {
                if self.does_occur_b(tvr, base)? {
                    return Ok(true);
                }
                for arg in args {
                    if self.does_occur_b(tvr, arg)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }

            Type::TypeFunction {
                parameters,
                return_type,
            } => {
                if self.does_occur_b(tvr, return_type)? {
                    return Ok(true);
                }
                for param in parameters {
                    if self.does_occur_b(tvr, param)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }

            _ => Ok(false),
        }
    }

    fn unifies_with(&mut self, ty1: Type, ty2: Type, base_ty1: Type, base_ty2: Type) -> Result<()> {
        if ty1 == ty2 {
            return Ok(());
        }

        if let Type::TypeVariable(var) = &ty1 {
            let mut mut_var = var.borrow_mut();
            if let TypeVariable::Bound(ty1) = mut_var.clone() {
                return self.unifies_with(*ty1, ty2, base_ty1, base_ty2);
            } else if let TypeVariable::Unbound(name, _) = mut_var.clone() {
                if self.does_occur_b(&mut *mut_var, &ty2)? {
                    return Err(reality_error::RealityError::InfiniteType(name));
                }

                *mut_var = TypeVariable::Bound(Box::new(ty2.clone()));
                return Ok(());
            }
        }

        if let Type::TypeVariable(other_var) = ty2.clone() {
            let mut mut_other_var = other_var.borrow_mut();

            if let TypeVariable::Bound(ty2) = mut_other_var.clone() {
                return self.unifies_with(ty1, *ty2, base_ty1, base_ty2);
            } else if let TypeVariable::Unbound(name, _) = mut_other_var.clone() {
                if self.does_occur_b(&mut *mut_other_var, &ty1)? {
                    return Err(reality_error::RealityError::InfiniteType(name));
                }

                *mut_other_var = TypeVariable::Bound(Box::new(ty1.clone()));
                return Ok(());
            }
        }

        if let Type::TypeApplication(base1, args1) = ty1.clone()
            && let Type::TypeApplication(base2, args2) = ty2.clone()
        {
            if base1 == base2 && args1.len() == args2.len() {
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.unifies_with(
                        arg1.clone(),
                        arg2.clone(),
                        base_ty1.clone(),
                        base_ty2.clone(),
                    )?;
                }
                return Ok(());
            }
        }

        if let Type::TypeFunction {
            parameters,
            return_type,
        } = ty1.clone()
            && let Type::TypeFunction {
                parameters: other_parameters,
                return_type: other_return_type,
            } = ty2.clone()
        {
            if parameters.len() == other_parameters.len() {
                for (param, other_param) in parameters.iter().zip(other_parameters.iter()) {
                    self.unifies_with(
                        param.clone(),
                        other_param.clone(),
                        base_ty1.clone(),
                        base_ty2.clone(),
                    )?;
                }
                return self.unifies_with(
                    *return_type.clone(),
                    *other_return_type.clone(),
                    base_ty1,
                    base_ty2,
                );
            }
        }

        Err(reality_error::RealityError::TypeMismatch(
            base_ty1, base_ty2,
        ))
    }
}
