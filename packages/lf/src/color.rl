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
            30u64
        } else if c is Red {
            31u64
        } else if c is Green {
            32u64
        } else if c is Yellow {
            33u64
        } else if c is Blue {
            34u64
        } else if c is Magenta {
            35u64
        } else if c is Cyan {
            36u64
        } else if c is White {
            37u64
        } else {
            39u64
        };

    let brightness_offset = 
        if b is Normal {
            0u64
        } else {
            60u64
        };

    let background_offset = if bg is Foreground {
        0u64
    } else {
        10u64
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


