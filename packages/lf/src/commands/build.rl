import core.src.configuration;
import core.src.cli;
import core.src.color;
import core.src.parser;
import core.src.commands;

enum Mode {
    Dev,
    Release
}

fn parse_key_value_as_alias(kv: Configuration) -> Option<String> {
    if kv is KeyValue(let key, Str(let value)) {
        return Some(key + "=" + value);
    } else {
        return None;
    }
}

impl fn (m: Mode) show_prec(prec: int) -> String {
    if m is Dev {
        return "Development Mode";
    } else if m is Release {
        return "Release Mode";
    } else {
        return "Unknown Mode";
    }
}

fn build_aliases(aliases: List<Option<String>>) -> String {
    let result = "";

    let i = 0;
    while i < aliases.length {
        let aliasOpt = aliases[i];
        if aliasOpt is Some(let aliasStr) {
            result = result + " -p " + aliasStr;
        };
        i = i + 1;
    };

    return result;
}

fn get_libc_flags(config: List<*Configuration>) -> String {
    let libc_section = config.get_section(["features", "libc"]);
    let libraries = if libc_section is Some(Section(_, let configs)) && configs.get_key_value("libraries") is Some(Arr(let flags)) {
        flags
    } else {
        []
    };

    let libs = [
        "$REALITY_DIR/libc/String.c",
        "$REALITY_DIR/libc/Actor.c",
        "$LIBGC/lib/libgc.a",
        "-I$LIBGC/include/"
    ].extend(libraries);

    return libs.join(" ");
}

fn build_command(cwd: String, args: List<CLI>, config: List<*Configuration>) -> Option<String> {
    let mode = if args.has_flag("dev") { Dev } else { Release };

    print_lf("Building project in " + mode.show() + "...");

    let package = config.get_section(["package"]);
    
    let fields = if package is Some(Section(_, let configs)) {
        configs
    } else {
        []
    };

    let main_file = if fields.get_key_value("main") is Some(Str(let mf)) {
        mf
    } else {
        "main.rl"
    };

    let aliases = config.get_section(["features", "aliases"]);
    let alias_fields = if aliases is Some(Section(_, let _configs)) {
        _configs
    } else {
        []
    };
    let alias_kvs = alias_fields.map(|c| parse_key_value_as_alias(*c));
    let aliases_str = build_aliases(alias_kvs);

    let headers = config.get_section(["features", "libc"]);
    let header_fields = if headers is Some(Section(_, let configs2)) {
        configs2
    } else {
        []
    };

    let header_kvs = header_fields.get_key_value("headers");

    let headers_values = if header_kvs is Some(Arr(let hdrs)) {
        hdrs
    } else {
        []
    };

    let headers_str = headers_values.map(|h: String| "-I \"" + h + "\"").join(" ");

    print_lf("Main file: " + color(Black) + main_file + reset_code());
    print_lf("Aliases:" + color(Black) + aliases_str + reset_code());
    print_lf("Mode: " + color(Black) + mode.show_prec(0) + reset_code());
    print_lf("Current working directory: " + color(Black) + cwd + reset_code());

    let compile_command_rlc = "rlc " + cwd + "/" + main_file + aliases_str + " " + headers_str;

    let exit_code = execute_command(compile_command_rlc.data);

    if exit_code != 0 {
        print_error_lf("Build failed with exit code " + (exit_code as int).show_prec(0) + ".");

        return None;
    }

    let main_file_split = main_file.split('.');
    let c_file = main_file_split.slice(0, main_file_split.length - 1).join(".") + ".c";

    let c_file_location = cwd + "/" + c_file;

    let base_gcc_command = "gcc " + c_file_location + " " + get_libc_flags(config) + " -lm -w -o " + cwd + "/output/program";

    let gcc_command = if mode is Dev {
        base_gcc_command + " -DDEV_MODE -g"
    } else {
        base_gcc_command + " -O3 -DRELEASE_MODE"
    };

    print_lf("Compiling with command: " + color(Black) + gcc_command + reset_code());

    if not(folder_exists((cwd + "/output").data)) {
        execute_command(("mkdir " + cwd + "/output").data);
    }

    let gcc_exit_code = execute_command(gcc_command.data);

    if gcc_exit_code != 0 {
        print_error_lf("GCC compilation failed with exit code " + (gcc_exit_code as int).show_prec(0) + ".");

        return None;
    }

    print_success_lf("Build succeeded! Output binary: " + color(Black) + "output/program" + reset_code());

    return Some(cwd + "/output/program");
}
