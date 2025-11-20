import std.string;
import std.parser;
import std.io;

enum CLI {
    PositionalArg(String),
    Flag(String),
    Option(String, String)
}

impl fn (cli: CLI) show_prec(i: i32) -> String {
    if cli is PositionalArg(let arg) {
        show_prec(arg, 0)
    } else if cli is Flag(let flag) {
        "-" + show_prec(flag, 0)
    } else if cli is Option(let key, let value) {
        "-" + show_prec(key, 0) + "=" + show_prec(value, i + 1)
    } else {
        "UnknownCLI"
    }
}

fn parse_positional_arg() -> Parser<CLI> {
    return satisfy(|c| is_alphanumeric(c) || c == '-' || c == '_')
        .some()
        .map(|chars| {
            let result = String.from_chars(chars);

            return PositionalArg(result);
        });
}

fn parse_flag() -> Parser<CLI> {
    let prefix = choice([string("--"), string("-")]);
    
    return prefix
        .after(
            satisfy(|c| is_alphanumeric(c) || c == '-' || c == '_')
                .some()
                .map(|chars| {
                    let result = String.from_chars(chars);

                    return Flag(result);
                })
        );
}

fn parse_option() -> Parser<CLI> {
    let prefix = choice([string("--"), string("-")]);

    return prefix
        .after(
            satisfy(|c| is_alphanumeric(c) || c == '-' || c == '_')
                .some()
                .before(
                    character('=')
                )
                .bind(|keyChars| {
                    let keyStr = String.from_chars(keyChars);
                    return satisfy(|c| c != '\n').some().map(|valueChars| {
                        let valueStr = String.from_chars(valueChars);
                        return Option(keyStr, valueStr);
                    });
                })
        );
}

fn parse_cli_arg() -> Parser<CLI> {
    return choice([parse_option(), parse_flag().try(), parse_positional_arg()]);
}

fn parse_cli_args() -> Parser<List<CLI>> {
    return parse_cli_arg()
        .skip_whitespace()
        .many();
}

impl fn (args: List<String>) parse_as_cli() -> List<CLI> {
    let cliStr = args.join(" ");
    let result = parse_cli_args()(cliStr);

    if (result) is Success(let cli_args, let _) {
        return cli_args;
    } else {
        return [];
    };
}

impl fn (l: List<CLI>) get_first_positional() -> Option<String> {
    let i = 0u64;
    while i < l.length {
        let cli_arg = l[i];
        if cli_arg is PositionalArg(let arg) {
            return Some(arg);
        };
        i = i + 1;
    };

    return None;
}

impl fn (l: List<CLI>) has_flag(flagName: String) -> bool {
    let i = 0u64;
    while i < l.length {
        let cli_arg = l[i];
        if cli_arg is Flag(let name) {
            if name == flagName {
                return true;
            };
        };
        i = i + 1;
    };

    return false;
}

impl fn (l: List<CLI>) get_option(optionName: String) -> Option<String> {
    let i = 0u64;
    while i < l.length {
        let cli_arg = l[i];
        if cli_arg is Option(let name, let value) {
            if name == optionName {
                return Some(value);
            };
        };
        i = i + 1;
    };

    return None;
}

type Map<K, V> = List<Tuple<K, V>>;

impl fn (m: Map<K, V>) get<K, V>(key: K) -> Option<V> {
    let i = 0u64;
    while i < m.length {
        if m[i] is Pair(let k, let v) && k == key {
            return Some(v);
        }

        i = i + 1;
    };

    return None;
}

impl fn (m: Map<K, V>) has_key<K, V>(key: K) -> bool {
    let i = 0u64;
    while i < m.length {
        if m[i] is Pair(let k, let v) && k == key {
            return true;
        }
        i = i + 1;
    };

    return false;
}

impl fn (it: Map<K, V>) iter<K, V>() -> Iterator<Map<K, V>> {
    struct Iterator<Map<K, V>> {
        container: it,
        index: GC.allocate(0u64)
    }
}

impl fn (it: Iterator<Map<K, V>>) next<K, V>() -> Option<Tuple<K, V>> {
    if *it.index < it.container.length {
        let value = it.container.at(*it.index);
        *it.index = *it.index + 1u64;
        Some(value)
    } else {
        None
    }
}
