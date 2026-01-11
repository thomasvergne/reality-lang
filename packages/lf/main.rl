import std.string;
import std.parser;
import std.io;
import std.iterator;
import source.parser;
import source.configuration;
import source.cli;
import source.color;

// Commands
import source.commands.build;
import source.commands.help;
import source.commands.init;

fn get_cwd() -> String {
    let cwd_str = get_current_working_directory();
    return struct String {
        data: cwd_str,
        length: strlen(cwd_str)
    };
}

type Command = fn(String, List<CLI>) -> unit;
type Commands = Map<String, Command>;


fn run_command(cwd: String, args: List<CLI>, config: List<*Configuration>) -> unit {
    let build_output = build_command(cwd, args, config);

    if build_output is Some(let binary_path) {
        LF.log("Running program...");

        let run_exit_code = execute_command(binary_path.data);

        if run_exit_code != 0 {
            LF.error("Program exited with code " + (run_exit_code as int).show_prec(0) + ".");
        } else {
            LF.success("Program executed successfully.");
        }
    } else {
        LF.error("Build failed; cannot run program.");
    }

    unit
}

fn get_commands(cwd: String, config: List<*Configuration>) -> Commands {
    return [
        ("build", |cwd, args| {
            build_command(cwd, args, config);
            return unit
        }),
        ("help", help_command),
        ("run", |cwd, args| run_command(cwd, args, config)),
        ("init", init_command)
    ];
}

pub fn main(args: List<String>) -> int {
    let cli_args = args.slice(1, args.length).parse_as_cli();

    let command = cli_args.get_first_positional();

    let cwd = get_cwd();
    if command is Some(let cmd1) && cmd1 == "init" {
        init_command(cwd, cli_args.slice(1, cli_args.length));
        return 0;
    };

    let config = Configuration.parse_file(cwd + "/config.toml");

    let final_config = if config is Ok(let conf) {
        conf 
    } else if config is Err(let errMsg) {
        LF.error(errMsg);
        GC.exit(1);
    } else {
        LF.error("Unknown error while parsing configuration.");
        GC.exit(1);
    }

    if command is Some(let cmd) {
        let commands = get_commands(cwd, final_config);
        let cmd_fn = commands.get(cmd);

        if cmd_fn is Some(let f) {
            f(cwd, cli_args.slice(1, cli_args.length));

            0
        } else {
            print("Unknown command: " + cmd);
        }
    } else {
        print("No positional argument (command) provided.");
    }

    return 0;
}
