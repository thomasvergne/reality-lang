mod string {
    type string = *char;
}

property add[A](x: A, y: A) -> A;

struct String {
    data: string,
    length: u32
};

extern fn malloc_string(s: string) -> string;
extern fn strlen(s: string) -> u32;
extern fn strcat(x: string, y: string) -> string;

mod String {
    fn new(data: string) -> String {
        let string_mem = malloc_string(data);

        String {
            data: string_mem,
            length: strlen(string_mem)
        }
    }
}

impl fn (x: String) add(y: String) -> String {
    let result = strcat(x.data, y.data);

    String {
        data: result,
        length: strlen(result)
    }
}

extern fn printf(s: string) -> i32;

property show[A](x: A) -> String;

impl fn (x: String) show() -> String {
    x
}

impl fn (x: string) show() -> String {
    String::new(x)
}

fn print[A](x: A) -> i32 {
    printf((show(x) + String::new("\n")).data)
}

impl fn (x: string) add(y: string) -> string {
    (String::new(x) + String::new(y)).data
}

extern fn number_to_string(n: i32) -> string;
extern fn add_number(a: i32, b: i32) -> i32;

impl fn (x: i32) show() -> String {
    String::new(number_to_string(x))
}

impl fn (x: i32) add(y: i32) -> i32 {
    add_number(x, y)
}
