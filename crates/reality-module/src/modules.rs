use reality_ast::{internal::annotation::Annotation, ToplevelNode};
use reality_error::RealityError;

pub struct ModuleResolver<'a> {
    pub file: &'a str,
    pub input: &'a str,
    pub location: (usize, usize),
}

type Result<T> = std::result::Result<T, RealityError>;

impl<'a> ModuleResolver<'a> {
    pub fn new(file: &'a str, input: &'a str) -> Self {
        ModuleResolver { file, input, location: (0, 0) }
    }

    pub fn resolve(&self, ast: Vec<ToplevelNode>) -> Result<Vec<ToplevelNode>> {
        return self.resolve_modules(ast, vec![]);
    }
    
    fn create_name(&self, paths: Vec<String>, name: String) -> String {
        if paths.is_empty() {
            name
        } else {
            format!("{}::{}", paths.join("::"), name)
        }
    }

    fn resolve_modules(&self, nodes: Vec<ToplevelNode>, paths: Vec<String>) -> Result<Vec<ToplevelNode>> {
        let mut resolved_nodes = Vec::new();

        for node in nodes {
            let resolved = self.resolve_singular(node, paths.clone())?;
            resolved_nodes.extend(resolved);
        }

        Ok(resolved_nodes)
    }

    fn resolve_singular(&self, node: ToplevelNode, mut paths: Vec<String>) -> Result<Vec<ToplevelNode>> {
        match node {
            ToplevelNode::ConstantDeclaration { variable, value } => {
                // Resolve the constant declaration

                let new_name = self.create_name(paths, variable.name);

                Ok(vec![ToplevelNode::ConstantDeclaration { variable: Annotation {
                    name: new_name,
                    value: variable.value,
                    location: variable.location,
                }, value: value }])
            }

            ToplevelNode::FunctionDeclaration { name, parameters, return_type, body } => {
                let new_name = self.create_name(paths, name.name);

                Ok(vec![ToplevelNode::FunctionDeclaration { name: Annotation {
                    name: new_name,
                    value: name.value,
                    location: name.location,
                }, parameters, return_type, body }])
            }

            ToplevelNode::PublicDeclaration(inner) => 
                self.resolve_singular(*inner, paths),

            ToplevelNode::TypeAlias { name, body } => {
                let new_name = self.create_name(paths, name.name);

                Ok(vec![ToplevelNode::TypeAlias { name: Annotation {
                    name: new_name,
                    value: name.value,
                    location: name.location,
                }, body }])
            }

            ToplevelNode::ModuleDeclaration { name, body } => {
                paths.push(name.clone());
                let resolved = self.resolve_modules(body, paths.clone())?;
                paths.pop();

                Ok(resolved)
            }

            ToplevelNode::ImportDeclaration(_) => 
                Err(RealityError::NoRequireStatement),
        }
    }
}
