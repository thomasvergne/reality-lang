import std::string::*;
import std::list;
import actor::*;
import std::option;
import std::linked_list::*;
import std::iterator;
import std::tuple;

fn main(args: List[String]) -> i32 {
    let s = Some(Some("test"));

    if s is Some(let value) && value is Some(let innerValue) {
        print("Some test");
    } else {
        print("None or different");
    }

    print(s);

    return 0;
}
