use std::fs::write;

use reality_anf::ANF;
use reality_cgen::CodeGeneration;
use reality_closure::{hoisting::Hoister, ClosureConverter};
use reality_error::report_error;
use reality_module::{imports::ImportResolver, modules::ModuleResolver};
use reality_parser::{Parser, add_default_operators};
use reality_specialize::Specializer;
use reality_typechecker::Typechecker;

fn main() {
    let file = "examples/structures.rl";
    let c_imports = vec!["<stdint.h>", "<stdlib.h>", "\"dependencies/gc/src/gc.h\""];
    let file_content = include_str!("../examples/structures.rl");
    let current_dir = std::env::current_dir().unwrap().join("examples");

    let mut parser = Parser::new(file_content, file);
    add_default_operators(&mut parser);

    let result = parser.parse_program();

    if let Err(err) = &result {
        return report_error(
            file,
            (parser.position, parser.position + 1),
            err.clone(),
        );
    }

    let ast = result.unwrap();

    let mut import_resolver = ImportResolver::new(
        file,
        file_content,
        current_dir.to_str().unwrap().to_string(),
    );
    let ast = import_resolver.resolve_all(ast);

    if let Err(err) = &ast {
        return report_error(
            import_resolver.file,
            (import_resolver.position.0, import_resolver.position.1),
            err.clone(),
        );
    }

    let ast = ast.unwrap();

    let module_resolver = ModuleResolver::new(file, file_content);
    let result = module_resolver.resolve(ast);

    if let Err(err) = result {
        return report_error(
            module_resolver.file,
            (module_resolver.location.0, module_resolver.location.1),
            err.clone(),
        );
    }

    let ast = result.unwrap();

    let mut typechecker = Typechecker::new(file_content, file);

    let result = typechecker.check_program(ast);

    if let Err(err) = result {
        return report_error(
            typechecker.file,
            typechecker.position,
            err.clone(),
        );
    }

    let ast = result.unwrap();

    let mut specializer = Specializer::new(&mut typechecker);
    let result = specializer.specialize(ast);

    if let Err(err) = result {
        return report_error(
            specializer.source.2.as_str(),
            (specializer.source.0, specializer.source.1),
            err.clone(),
        );
    }

    let ast = result.unwrap();

    let mut closure_converter = ClosureConverter::new();
    let result = closure_converter.convert(ast);

    if let Err(err) = result {
        return report_error(
            closure_converter.source.2.as_str(),
            (closure_converter.source.0, closure_converter.source.1),
            err.clone(),
        );
    }

    let ast = result.unwrap();

    let hoister = Hoister::new();

    let hoisted_ast = hoister.hoist(ast);

    let mut anf_converter = ANF::new();
    let ast = anf_converter.compile(hoisted_ast);

    let code_generator = CodeGeneration::new();
    let result = code_generator.generate(ast);

    let mut final_code = c_imports.iter().map(|s| format!("#include {}\n", s)).collect::<Vec<_>>().join("");
    final_code.push_str(&result);

    let result = write("output.c", final_code);

    if let Err(err) = result {
        eprintln!("Error writing to output.c: {}", err);
    }
}
