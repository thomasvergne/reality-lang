import std.string;

extern fn read_file_ext(path: string) -> string;

fn read_file(path: String) -> String {
    let content = read_file_ext(path.data);
    
    return String.init(content);
}
