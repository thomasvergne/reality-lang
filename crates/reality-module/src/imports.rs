use std::{
    cell::RefCell,
    path::{Path, PathBuf},
};

use reality_ast::ToplevelNode;
use reality_error::RealityError;
use reality_parser::add_default_operators;

#[derive(Debug, Clone, PartialEq)]
pub struct ImportResolver<'a> {
    modules: Vec<Module>,
    pub position: (usize, usize),

    pub file: &'a str,
    pub input: &'a str,

    cwd: String,
}

#[derive(Debug, Clone, PartialEq)]
struct Module {
    absolute_path: String,
    package_name: String,
    visited: RefCell<State>,
    is_main: bool,

    result: Vec<ToplevelNode>,
}

#[derive(Debug, Clone, PartialEq)]
enum State {
    Unvisited = 0,
    Visiting = 1,
    Visited = 2,
}

impl<'a> ImportResolver<'a> {
    pub fn new(file: &'a str, input: &'a str, cwd: String) -> Self {
        Self {
            modules: Vec::new(),
            position: (0, 0),
            file,
            input,
            cwd,
        }
    }

    fn add_module(
        &mut self,
        absolute_path: String,
        package_name: String,
        is_main: bool,
        modules: Vec<ToplevelNode>,
    ) {
        self.modules.push(Module {
            absolute_path,
            package_name,
            visited: RefCell::new(State::Unvisited),
            is_main,
            result: modules,
        });
    }

    fn find_module(&self, package_name: String) -> Option<&Module> {
        self.modules.iter().find(|m| m.package_name == package_name)
    }

    fn visit_module(
        &mut self,
        absolute_path: PathBuf,
        package_name: String,
        should_flatten_module: bool,
    ) -> Result<Vec<ToplevelNode>, RealityError> {
        // If the module is not found, we need to read it from the file system
        if !absolute_path.exists() {
            return Err(RealityError::ModuleNotFound(package_name.clone()));
        }

        // Read the file content
        let file_content = std::fs::read_to_string(&absolute_path)
            .map_err(|_| RealityError::ModuleNotFound(package_name.clone()))?;

        let old_file = self.file;
        let old_input = self.input;

        self.file = Box::leak(
            absolute_path
                .to_string_lossy()
                .into_owned()
                .into_boxed_str(),
        );
        self.input = Box::leak(file_content.clone().into_boxed_str());

        // Parse the file content
        let mut parser = reality_parser::Parser::new(&file_content, &absolute_path.to_string_lossy());
        add_default_operators(&mut parser);
        let ast = parser.parse_program();

        match ast {
            Ok(ast) => {
                // Add the module to the resolver
                self.add_module(
                    absolute_path.to_string_lossy().to_string(),
                    package_name.clone(),
                    false,
                    ast,
                );

                // Mark the module as visited
                if let Some(module) = self.clone().find_module(package_name.clone()) {
                    module.visited.replace(State::Visited);

                    self.file = old_file;
                    self.input = old_input;

                    if should_flatten_module {
                        let flattened = self.flatten_one_level(module.result.clone());
                        return Ok(flattened);
                    } else {
                        return Ok(module.result.clone());
                    }
                } else {
                    self.position = (parser.position, parser.position + 1);
                    return Err(RealityError::ModuleNotFound(package_name));
                }
            }
            Err(err) => {
                self.position = (parser.position, parser.position + 1);
                return Err(err);
            }
        }
    }

    fn resolve(
        &mut self,
        module_path: String,
        package_name: String,
        should_flatten_module: bool,
    ) -> Result<Vec<ToplevelNode>, RealityError> {
        let path = Path::new(&self.cwd);
        let absolute_path = path.join(module_path);

        if let Some(ref module) = self.find_module(package_name.clone()) {
            match module.visited.clone().get_mut() {
                State::Unvisited => {
                    // Mark as visiting
                    module.visited.replace(State::Visiting);

                    return self.visit_module(absolute_path, package_name, should_flatten_module);
                }
                State::Visiting => {
                    return Err(RealityError::CycleDetected(module.absolute_path.clone()));
                }
                State::Visited => return Ok(module.result.clone()),
            }
        } else {
            // If the module is not found, we need to visit it
            return self.visit_module(absolute_path, package_name, should_flatten_module);
        }
    }

    pub fn resolve_all(
        &mut self,
        ast: Vec<ToplevelNode>,
    ) -> Result<Vec<ToplevelNode>, RealityError> {
        let mut resolved_modules = Vec::new();

        for module in ast.iter() {
            let old_location = self.position;

            if let ToplevelNode::Located { node, span } = module {
                let old_location: (usize, usize) = self.position;
                let old_file = self.file;
                self.position = (span.0, span.1);
                self.file = Box::leak(span.clone().2.into_boxed_str());
                
                if let ToplevelNode::ImportDeclaration(paths) = *node.clone() {
                    let mut paths = paths.clone();
                    let mut should_flatten_module = false;

                    if paths.ends_with(&["*".to_string()]) {
                        paths.pop();
                        should_flatten_module = true;
                    }

                    let module_name_str = paths.join("/");
                    let package_name = PathBuf::from(module_name_str.clone()).with_extension("rl");

                    let resolved = self.resolve(
                        package_name.to_string_lossy().into_owned(),
                        module_name_str,
                        should_flatten_module,
                    )?;

                    resolved_modules.extend(resolved);
                } else {
                    resolved_modules.push(module.clone());
                }

                self.position = old_location;
                self.file = old_file;
            } else if let ToplevelNode::ImportDeclaration(paths) = module {
                let mut paths = paths.clone();
                let mut should_flatten_module = false;

                if paths.ends_with(&["*".to_string()]) {
                    paths.pop();
                    should_flatten_module = true;
                }

                let module_name_str = paths.join("/");
                let package_name = PathBuf::from(module_name_str.clone()).with_extension("rl");

                let resolved = self.resolve(
                    package_name.to_string_lossy().into_owned(),
                    module_name_str,
                    should_flatten_module,
                )?;

                resolved_modules.extend(resolved);
            } else {
                // If it's not an import declaration, just keep the original module
                resolved_modules.push(module.clone());
            }

            self.position = old_location;
        }

        Ok(resolved_modules)
    }

    fn flatten_one_level(&self, ast: Vec<ToplevelNode>) -> Vec<ToplevelNode> {
        ast.into_iter()
            .flat_map(|node| {
                if let ToplevelNode::ModuleDeclaration { body, .. } = node {
                    body
                } else {
                    vec![node]
                }
            })
            .collect()
    }
}
