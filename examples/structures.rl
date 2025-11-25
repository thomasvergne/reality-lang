import std.string;
import std.list;
import std.option;
import std.iterator;
import std.tuple;
import std.error;

fn main(args: List<String>) -> int {
    print(args.slice(1, args.length));

    print(f"{args}, {args.slice(1, args.length)}");

    return 0;
}
