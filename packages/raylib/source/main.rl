import std.prelude;

mod Math {
    extern fn random_int(min: int, max: int) -> int;

    fn clamp<A>(value: A, min: A, max: A) -> A {
        if value < min {
            return min;
        }
        if value > max {
            return max;
        }
        return value;
    }

    fn random(min: int, max: int) -> int {
        random_int(min, max)
    }

    impl fn (x: int) negate() -> int {
        0 - x
    }
}

mod Bit {
    extern fn bshift_left(a: int, b: int) -> int;
    extern fn bshift_right(a: int, b: int) -> int;
    extern fn and_bit(a: int, b: int) -> int;
    extern fn or_bit(a: int, b: int) -> int;
    extern fn not_bit(a: int) -> int;
    
    impl fn (x: int) shift_left(y: int) -> int {
        bshift_left(x, y)
    }

    impl fn (x: int) shift_right(y: int) -> int {
        bshift_right(x, y)
    }

    impl fn (x: int) band(y: int) -> int {
        and_bit(x, y)
    }

    impl fn (x: int) bor(y: int) -> int {
        or_bit(x, y)
    }

    impl fn (x: int) bnot() -> int {
        not_bit(x)
    }
}

struct RGBA {
    r: int,
    g: int,
    b: int,
    a: int
}

mod Ray {
    extern fn init_window(width: int, height: int, title: string) -> void;
    extern fn set_fps(fps: int) -> void;
    extern fn should_close() -> bool;
    extern fn begin_drawing() -> void;
    extern fn clear_background_color(r: int, g: int, b: int, a: int) -> void;
    extern fn draw_text(
        text: string, 
        x: int, 
        y: int, 
        font_size: int, 
        r: int, 
        g: int,
        b: int, 
        a: int
    ) -> void;
    extern fn end_drawing() -> void;
    extern fn close_window() -> void;
    extern fn draw_rectangle(
        x: int, 
        y: int, 
        width: int, 
        height: int, 
        r: int, 
        g: int, 
        b: int, 
        a: int
    ) -> void;

    extern fn get_mouse_x() -> int;
    extern fn get_mouse_y() -> int;

    extern fn get_mouse_wheel() -> float;
    extern fn keyboard_input() -> int;
    extern fn is_key_down(keycode: int) -> bool;

    extern fn random_int(min: int, max: int) -> int;
    extern fn get_screen_width() -> int;
    extern fn get_screen_height() -> int;

    fn width() -> int {
        get_screen_width()
    }

    fn height() -> int {
        get_screen_height()
    }

    fn keycode() -> int {
        keyboard_input()
    }

    fn is_key_down(keycode: int) -> bool {
        is_key_down(keycode)
    }

    fn from_hex(color: int) -> RGBA {
        let r = color.shift_right(24).band(0xFF);
        let g = color.shift_right(16).band(0xFF);
        let b = color.shift_right(8).band(0xFF);
        let a = color.band(0xFF);

        struct RGBA {
            r: r,
            g: g,
            b: b,
            a: a
        }
    }

    fn to_hex(color: RGBA) -> int {
        let r = color.r.shift_left(24);
        let g = color.g.shift_left(16);
        let b = color.b.shift_left(8);
        let a = color.a.shift_left(0);

        return r.bor(g).bor(b).bor(a);
    }

    fn from_rgbAa(r: int, g: int, b: int, a: int) -> RGBA {
        struct RGBA {
            r: r,
            g: g,
            b: b,
            a: a
        }
    }

    fn init(width: int, height: int, title: String) -> unit {
        init_window(width, height, title.data);

        unit
    }

    fn set_fps(fps: int) -> unit {
        set_fps(fps);

        unit
    }

    fn is_running() -> bool {
        not(should_close())
    }

    fn with_drawing(f: fn() -> unit) -> unit {
        begin_drawing();
        f();
        end_drawing();

        unit
    }

    fn clear(color: RGBA) -> unit {
        let struct RGBA { r, g, b, a } = color;
        clear_background_color(r, g, b, a);

        unit
    }

    mod Circle {
        extern fn draw_circle(
            centerX: int,
            centerY: int,
            radius: float,
            r: int,
            g: int,
            b: int,
            a: int
        ) -> void;

        struct Circle {
            x: int,
            y: int,
            radius: float,
            color: RGBA
        }

        fn init() -> Ray.Circle.Circle {
            struct Ray.Circle.Circle {
                x: 0,
                y: 0,
                radius: 0.0,
                color: Ray.from_hex(0xFFFFFFFF)
            }
        }

        impl fn (circle: Ray.Circle.Circle) position(x: int, y: int) -> Ray.Circle.Circle {
            struct Ray.Circle.Circle {
                x: x,
                y: y,
                radius: circle.radius,
                color: circle.color
            }
        }

        impl fn (circle: Ray.Circle.Circle) radius(r: float) -> Ray.Circle.Circle {
            struct Ray.Circle.Circle {
                x: circle.x,
                y: circle.y,
                radius: r,
                color: circle.color
            }
        }

        impl fn (circle: Ray.Circle.Circle) color(clr: RGBA) -> Ray.Circle.Circle {
            struct Ray.Circle.Circle {
                x: circle.x,
                y: circle.y,
                radius: circle.radius,
                color: clr
            }
        }

        impl fn (circle: Ray.Circle.Circle) draw() -> unit {
            let struct RGBA { r, g, b, a } = circle.color;
            draw_circle(circle.x, circle.y, circle.radius, r, g, b, a);

            unit
        }
    }

    mod Rectangle {
        struct Rectangle {
            x: int,
            y: int,
            width: int,
            height: int,
            color: RGBA
        }

        fn init() -> Ray.Rectangle.Rectangle {
            struct Ray.Rectangle.Rectangle {
                x: 0,
                y: 0,
                width: 0,
                height: 0,
                color: Ray.from_hex(0xFFFFFFFF)
            }
        }

        impl fn (rect: Ray.Rectangle.Rectangle) position(x: int, y: int) -> Ray.Rectangle.Rectangle {
            struct Ray.Rectangle.Rectangle {
                x: x,
                y: y,
                width: rect.width,
                height: rect.height,
                color: rect.color
            }
        }

        impl fn (rect: Ray.Rectangle.Rectangle) size(width: int, height: int) -> Ray.Rectangle.Rectangle {
            struct Ray.Rectangle.Rectangle {
                x: rect.x,
                y: rect.y,
                width: width,
                height: height,
                color: rect.color
            }
        }

        impl fn (rect: Ray.Rectangle.Rectangle) color(clr: RGBA) -> Ray.Rectangle.Rectangle {
            struct Ray.Rectangle.Rectangle {
                x: rect.x,
                y: rect.y,
                width: rect.width,
                height: rect.height,
                color: clr
            }
        }

        impl fn (rect: Ray.Rectangle.Rectangle) draw() -> unit {
            draw_rectangle(rect.x, rect.y, rect.width, rect.height, rect.color.r, rect.color.g, rect.color.b, rect.color.a);

            unit
        }
    }

    mod Collision {
        extern fn check_collision_recs(
            rect1_x: int,
            rect1_y: int,
            rect1_width: int,
            rect1_height: int,
            rect2_x: int,
            rect2_y: int,
            rect2_width: int,
            rect2_height: int
        ) -> bool;

        extern fn check_collision_circles(
            x1: float,
            y1: float,
            radius1: float,
            x2: float,
            y2: float,
            radius2: float
        ) -> bool;

        extern fn check_collision_circle_rec(
            centerX: float,
            centerY: float,
            radius: float,
            rec_x: int,
            rec_y: int,
            rec_width: int,
            rec_height: int
        ) -> bool;

        fn rectangles(
            rect1: Ray.Rectangle.Rectangle,
            rect2: Ray.Rectangle.Rectangle
        ) -> bool {
            check_collision_recs(
                rect1.x,
                rect1.y,
                rect1.width,
                rect1.height,
                rect2.x,
                rect2.y,
                rect2.width,
                rect2.height
            )
        }

        fn circles(
            c1: Ray.Circle.Circle,
            c2: Ray.Circle.Circle
        ) -> bool {
            check_collision_circles(
                float(c1.x),
                float(c1.y),
                c1.radius,
                float(c2.x),
                float(c2.y),
                c2.radius
            )
        }

        fn circle_rectangle(
            circle: Ray.Circle.Circle,
            rec: Ray.Rectangle.Rectangle
        ) -> bool {
            check_collision_circle_rec(
                float(circle.x),
                float(circle.y),
                circle.radius,
                rec.x,
                rec.y,
                rec.width,
                rec.height
            )
        }
    }

    mod Text {
        extern fn text_width(text: string, font_size: int) -> int;

        struct Text {
            content: String,
            x: int,
            y: int,
            font_size: int,
            color: RGBA
        }

        fn init() -> Ray.Text.Text {
            struct Ray.Text.Text {
                content: "",
                x: 0,
                y: 0,
                font_size: 12,
                color: Ray.from_hex(0xFFFFFFFF)
            }
        }

        impl fn (txt: Ray.Text.Text) content(cnt: String) -> Ray.Text.Text {
            struct Ray.Text.Text {
                content: cnt,
                x: txt.x,
                y: txt.y,
                font_size: txt.font_size,
                color: txt.color
            }
        }

        impl fn (txt: Ray.Text.Text) position(x: int, y: int) -> Ray.Text.Text {
            struct Ray.Text.Text {
                content: txt.content,
                x: x,
                y: y,
                font_size: txt.font_size,
                color: txt.color
            }
        }

        impl fn (txt: Ray.Text.Text) font_size(size: int) -> Ray.Text.Text {
            struct Ray.Text.Text {
                content: txt.content,
                x: txt.x,
                y: txt.y,
                font_size: size,
                color: txt.color
            }
        }

        impl fn (txt: Ray.Text.Text) color(clr: RGBA) -> Ray.Text.Text {
            struct Ray.Text.Text {
                content: txt.content,
                x: txt.x,
                y: txt.y,
                font_size: txt.font_size,
                color: clr
            }
        }

        impl fn (txt: Ray.Text.Text) draw() -> unit {
            let struct RGBA { r, g, b, a } = txt.color;
            draw_text(txt.content.data, txt.x, txt.y, txt.font_size, r, g, b, a);

            unit
        }

        impl fn (txt: Ray.Text.Text) width() -> int {
            text_width(txt.content.data, txt.font_size)
        }
    }

    mod Mouse {
        fn x() -> int {
            get_mouse_x()
        }

        fn y() -> int {
            get_mouse_y()
        }

        fn wheel() -> float {
            get_mouse_wheel()
        }
    }

    fn close() -> unit {
        close_window();

        unit
    }

    fn randomColor() -> RGBA {
        let r = Math.random(0, 255);
        let g = Math.random(0, 255);
        let b = Math.random(0, 255);
        let a = 255;

        return struct RGBA { r: r, g: g, b: b, a: a };
    }
}
