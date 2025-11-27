import tuple;
import list;
import option;

pub extern fn is_whitespace(c: char) -> bool;
pub extern fn is_alpha(c: char) -> bool;
pub extern fn is_alphanumeric(c: char) -> bool;
pub extern fn is_digit(c: char) -> bool;

type Rest = String;

enum ParserResult<A> {
    Success(A, Rest),
    Failure(String, Rest)
}

type Parser<A> = fn(String) -> ParserResult<A>;

impl fn (r: ParserResult<A>) show_prec<A>(prec: int) -> String {
    if r is Success(let value, let rest) {
        let result = "Success(" + show_prec(value, prec + 1) + ", ";
        result = result + show_prec(rest, prec + 1) + ")";
        return result;
    } else if r is Failure(let errMsg, let rest) {
        let result = "Failure(" + show_prec(errMsg, prec + 1) + ", ";
        result = result + show_prec(rest, prec + 1) + ")";
        return result;
    } else {
        return "<invalid ParserResult>";
    }
}

impl fn (r: ParserResult<A>) or_else<A>(default: A) -> A {
    if r is Success(let value, let _) {
        return value;
    } else {
        return default;
    }
}

fn satisfy(predicate: fn(char) -> bool) -> Parser<char> {
    return |input| {
        if input.length == 0 {
            return Failure("Unexpected end of input", input);
        };

        let firstChar = input[0];
        if predicate(firstChar) {
            let rest = input.slice(1, input.length);
            return Success(firstChar, rest);
        } else {
            return Failure("Character did not satisfy predicate", input);
        };
    };
}

fn character(expected: char) -> Parser<char> {
    return satisfy(|c| c == expected);
}

impl fn (p: Parser<A>) then<A, B>(q: Parser<B>) -> Parser<(A, B)> {
    return |input| {
        if (p(input)) is Success(let value1, let rest1) {
            if (q(rest1)) is Success(let value2, let rest2) {
                return Success((value1, value2), rest2);
            } else if (q(rest1)) is Failure(let errMsg, let rest2) {
                return Failure(errMsg, rest2);
            };
        } else if (p(input)) is Failure(let errMsg, let rest1) {
            return Failure(errMsg, rest1);
        };
    }
}

impl fn (p: Parser<A>) map<A, B>(f: fn(A) -> B) -> Parser<B> {
    return |input| {
        if (p(input)) is Success(let value, let rest) {
            return Success(f(value), rest);
        } else if (p(input)) is Failure(let errMsg, let rest) {
            return Failure(errMsg, rest);
        };
    };
}

impl fn (p: Parser<A>) bind<A, B>(f: fn(A) -> Parser<B>) -> Parser<B> {
    return |input| {
        if (p(input)) is Success(let value, let rest) {
            let nextParser = f(value);
            return nextParser(rest);
        } else if (p(input)) is Failure(let errMsg, let rest) {
            return Failure(errMsg, rest);
        };
    };
}

impl fn (p: Parser<A>) many<A>() -> Parser<List<A>> {
    return |input| {
        let results = new List.init<A>();
        let currentInput = input;

        while p(currentInput) is Success(let value, let rest) {
            results.push(value);
            currentInput = rest;
        };

        return Success(*results, currentInput);
    };
}


fn whitespace() -> Parser<String> {
    return satisfy(is_whitespace).many().map(
        |chars| {
            let result = "";
            let i = 0;
            while i < chars.length {
                let c = chars.get_index(i);
                result = result + String.from_char(c);
                i = i + 1;
            };
            return result;
        }
    );
}

impl fn (p: Parser<A>) before<A, B>(q: Parser<B>) -> Parser<A> {
    return p.bind(|value| {
        return q.map(|_| value);
    });
}

impl fn (p: Parser<A>) after<A, B>(q: Parser<B>) -> Parser<B> {
    return p.bind(|_| q);
}

impl fn (p: Parser<A>) skip_whitespace<A>() -> Parser<A> {
    return whitespace()
        .after(
            p.before(
                whitespace()
            )
        );
}

impl fn (p: Parser<A>) try<A>() -> Parser<A> {
    return |input| {
        let result = p(input);
        if (result) is Success(let value, let rest) {
            return Success(value, rest);
        } else if (result) is Failure(let _1, let _2) {
            return Failure("Parser failed without consuming input", input);
        };
    };
}

impl fn (p: fn() -> Parser<A>) lazy<A>() -> Parser<A> {
    return |input| {
        let parser = p();
        return parser(input);
    };
}

fn choice<A>(parsers: List<Parser<A>>) -> Parser<A> {
    return |input| {
        let i = 0;

        while i < parsers.length {
            let parser = parsers[i];
            if parser(input) is Success(let value, let rest) {
                return Success(value, rest);
            }

            i = i + 1;
        };

        return Failure("All parsers failed", input);
    };
}

impl fn (p: Parser<A>) optional<A>() -> Parser<Option<A>> {
    return |input| {
        if (p(input)) is Success(let value, let rest) {
            return Success(Some(value), rest);
        } else {
            return Success(None, input);
        };
    };
}

impl fn (p: Parser<A>) some<A>() -> Parser<List<A>> {
    return |input| {
        if (p(input)) is Success(let firstValue, let rest) {
            let results = new List.init<A>();
            results.push(firstValue);

            let currentInput = rest;

            while (p(currentInput)) is Success(let value, let rest2) {
                results.push(value);
                currentInput = rest2;
            };

            return Success(*results, currentInput);
        } else {
            return Failure("Expected at least one match", input);
        };
    }
}

impl fn (p: Parser<A>) run<A>(input: String) -> ParserResult<A> {
    if (p(input)) is Success(let value, let rest) {
        if rest.length == 0 {
            return Success(value, rest);
        } else {
            return Failure("Parser did not consume entire input", rest);
        };
    } else if (p(input)) is Failure(let errMsg, let _) {
        return Failure(errMsg, input);
    };
}

pub impl fn (p: Parser<A>) sep_by<A, B>(sep: Parser<B>) -> Parser<List<A>> {
    return |input| {
        let results = new List.init<A>();

        if (p(input)) is Success(let firstValue, let rest1) {
            results.push(firstValue);
            let currentInput = rest1;

            while (sep(currentInput)) is Success(let _, let rest2) {
                if (p(rest2)) is Success(let value, let rest3) {
                    results.push(value);
                    currentInput = rest3;
                } else {
                    break;
                };
            };

            return Success(*results, currentInput);
        } else {
            return Success(*results, input);
        };
    };
}

fn string(s: String) -> Parser<String> {
    return |input| {
        let i = 0;

        while i < s.length {
            if input.length <= i {
                return Failure("Unexpected end of input", input);
            };

            if input[i] != s[i] {
                return Failure("String did not match", input);
            };

            i = i + 1;
        };

        let rest = input.slice(s.length, input.length);
        return Success(s, rest);
    };
}
