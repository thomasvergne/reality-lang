enum Color {
    Black,
    Red,
    Green,
    Blue,
    Yellow,
    Magenta,
    Cyan,
    White
}

enum Brightness {
    Normal,
    Bright
}

enum Background {
    Foreground,
    Background
}

enum Style {
    Plain,
    Bold,
    Underline,
    Reversed
}

fn color_code(c: Color, b: Brightness, bg: Background) -> String {
    let base = 
        if c is Black {
            30
        } else if c is Red {
            31
        } else if c is Green {
            32
        } else if c is Yellow {
            33
        } else if c is Blue {
            34
        } else if c is Magenta {
            35
        } else if c is Cyan {
            36
        } else if c is White {
            37
        } else {
            39
        };

    let brightness_offset = 
        if b is Normal {
            0
        } else {
            60
        };

    let background_offset = if bg is Foreground {
        0
    } else {
        10
    };

    return (base + brightness_offset + background_offset).show_prec(0);
}

pub fn ansi_code(c: Color, b: Brightness, bg: Background, s: Style) -> String {
    let style_code = if s is Plain {
        "0"
    } else if s is Bold {
        "1"
    } else if s is Underline {
        "4"
    } else if s is Reversed {
        "7"
    } else {
        "0"
    };

    let color_code_str = color_code(c, b, bg);

    return "\x1b[" + style_code + ";" + color_code_str + "m";
}

pub fn color(c: Color) -> String {
    return ansi_code(c, Normal, Foreground, Plain);
}

pub fn background(c: Color) -> String {
    return ansi_code(c, Normal, Background, Plain);
}

pub fn bright_color(c: Color) -> String {
    return ansi_code(c, Bright, Foreground, Plain);
}

pub fn bright_background(c: Color) -> String {
    return ansi_code(c, Bright, Background, Plain);
}

pub fn bold_color(c: Color) -> String {
    return ansi_code(c, Normal, Foreground, Bold);
}

pub fn underline_color(c: Color) -> String {
    return ansi_code(c, Normal, Foreground, Underline);
}

pub fn reversed_color(c: Color) -> String {
    return ansi_code(c, Normal, Foreground, Reversed);
}

pub fn reset_code() -> String {
    return "\x1b[0m";
}

fn print_lf<A>(value: A) -> unit {
    let str = value.show_prec(0);
    print(bold_color(Yellow) + "[LF]" + reset_code() + ": " + str)

    unit
}

fn print_error_lf<A>(value: A) -> unit {
    let str = value.show_prec(0);
    print(bold_color(Red) + "[LF]" + reset_code() + ": " + str)

    unit
}

fn print_success_lf<A>(value: A) -> unit {
    let str = value.show_prec(0);
    print(bold_color(Green) + "[LF]" + reset_code() + ": " + str)

    unit
}
