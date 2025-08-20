use reality_error::report_error;
use reality_parser::{add_default_operators, Parser};

fn main() {
    let file = "examples/main.rl";
    let file_content = include_str!("../examples/main.rl");

    let mut parser = Parser::new(file_content, file);
    add_default_operators(&mut parser);

    let result = parser.parse_program();

    if let Err(err) = &result {
        return report_error(file, file_content, (parser.position, parser.position + 1), err.clone());
    }

    let ast = result.unwrap();

    for node in &ast {
        println!("{:?}", node);
    }
}
