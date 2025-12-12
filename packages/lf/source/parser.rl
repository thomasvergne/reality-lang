import std.string;
import std.parser;
import std.io;
import configuration;
import std.error;

fn identifier() -> Parser<String> {
    return satisfy(is_alpha).bind(
        |firstChar| {
            return satisfy(is_alphanumeric).many().map(
                |restChars: List<char>| {
                    let idStr = String.init(new firstChar);
                    let result = idStr;
                    let i = 0;
                    while i < restChars.length {
                        let c = restChars[i];
                        result = result + String.init(new c);
                        i = i + 1;
                    };
                    return result;
                }
            );
        }
    )
}

fn parse_string() -> Parser<String> {
    return character('"')
        .after(
            satisfy(|c| c != '"').many().map(
                |chars: List<char>| {
                    let result = "";
                    let i = 0;
                    while i < chars.length {
                        let c = chars[i];
                        result = result + String.init(new c);
                        i = i + 1;
                    };
                    return result;
                }
            )
        )
        .before(character('"'));
}

fn parse_array() -> Parser<List<String>> {
    return character('[')
        .after(
            parse_string().sep_by(character(',').skip_whitespace())
        )
        .before(character(']'));
}

fn parse_value(key: String) -> Parser<*Configuration> {
    return choice([
        parse_string().map(
            |strValue| {
                return new KeyValue(key, Str(strValue));
            }
        ),
        parse_array().map(
            |arrValue| {
                return new KeyValue(key, Arr(arrValue));
            }
        )
    ]);
}

fn parse_configuration() -> Parser<*Configuration> {
    let sectionParser = character('[')
        .after(
            identifier().sep_by(character('.')).before(character(']'))
        )
        .skip_whitespace()
        .map(|sectionName| {
            return GC.allocate(Section(sectionName, []));
        });

    let keyValueParser = identifier()
        .skip_whitespace()
        .before(
            character('=')
                .skip_whitespace()
        )
        .bind(|key| {
            return parse_value(key).skip_whitespace();
        });

    choice([sectionParser, keyValueParser])
}

mod Configuration {
    extern fn file_exists(path: string) -> bool;

    pub fn parse_file(filename: String) -> Error<List<*Configuration>, String> {
        if not(file_exists(filename.data)) {
            return Err("Configuration file not found: " + filename);
        };

        let file_content = File.open(filename);

        let parse_result = parse_configuration().some().run(file_content);

        if parse_result is Success(let configs, let _) {
            return Ok(configs.build_tree());
        } else {  
            return Err("Failed to parse configuration file: " + filename);
        }
    }

    pub fn serialize(configs: List<*Configuration>) -> String {
        let result = "";
        let i = 0;
        while i < configs.length {
            let config = *configs[i];
            result = result + config.show_prec(0);

            if i + 1 < configs.length {
                result = result + "\n\n";
            };

            i = i + 1;
        };

        return result;
    }
}
