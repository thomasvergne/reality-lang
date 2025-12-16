import core.source.configuration;
import core.source.cli;
import core.source.color;
import core.source.parser;
import core.source.commands;

fn init_command(cwd: String, args: List<CLI>) -> unit {
    let project_name = args
        .get_option("name")
        .get_or_else(
            args
                .get_first_positional()
                .get_or_else("my_project")
        );

    LF.log(f"Initialized new LiFe project in " + color(Black) + f"{cwd}/{project_name}" + reset_code() + ".");

    let config = [
        new Section(
            ["package"],
            [
                new KeyValue("name", Str(project_name)),
                new KeyValue("version", Str("0.1.0")),
                new KeyValue("main", Str("source/main.rl"))
            ]
        ),

        new Section(
            ["features", "aliases"],
            [
                new KeyValue("std", Str("$REALITY_DIR/std"))
            ]
        )
    ];

    System.execute(f"mkdir -p {cwd}/{project_name}");
    System.execute(f"mkdir -p {cwd}/{project_name}/source");

    File.write(
        f"{cwd}/{project_name}/config.toml",
        Configuration.serialize(config)
    );

    File.write(
        f"{cwd}/{project_name}/source/main.rl",
        f"import std.prelude;\n\nfn main() -> unit \{\n    print(\"Hello, {project_name}!\");\n    return unit;\n}\n"
    );
    
    LF.success("Created default configuration and main source file.");

    unit
}
