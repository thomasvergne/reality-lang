import std.string;
import std.parser;
import std.io;
import configuration;

fn identifier() -> Parser<String> {
    return satisfy(is_alpha).bind(
        |firstChar| {
            return satisfy(is_alphanumeric).many().map(
                |restChars| {
                    let idStr = String.init(GC.allocate(firstChar));
                    let result = idStr;
                    let i = 0u64;
                    while i < restChars.length {
                        let c = restChars[i];
                        result = result + String.init(GC.allocate(c));
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
                |chars| {
                    let result = "";
                    let i = 0u64;
                    while i < chars.length {
                        let c = chars[i];
                        result = result + String.init(GC.allocate(c));
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
                return GC.allocate(KeyValue(key, Str(strValue)));
            }
        )
        ,
        parse_array().map(
            |arrValue| {
                return GC.allocate(KeyValue(key, Arr(arrValue)));
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
    pub fn parse_file(filename: String) -> Option<List<*Configuration>> {
        let file_content = read_file(filename);

        let parse_result = parse_configuration().some().run(file_content);

        if parse_result is Success(let configs, let _) {
            return Some(configs.build_tree());
        } else {
            return None;
        }
    }
}
