import std.string;
import std.list;
import actor;
import std.option;
import std.iterator;
import std.tuple;
import std.error;
import concurrent_queue;

enum Message {
    Print(i32),
    Stop
}

impl fn (msg: Message) show_prec(prec: i32) -> String {
    if msg is Print(let value) {
        return "Print(" + show_prec(value, prec + 1) + ")";
    };

    if msg is Stop {
        return "Stop";
    };
}

fn main(args: List<String>) -> i32 {
    let act = Actor.create<Message>(|act, msg| {
        if msg is Print(let value) {
            print(value);
        };

        unit
    })

    act.send(Print(42));
    act.send(Print(7));
    act.send(Print(13));
    act.send(Stop);

    act.wait();
    0
}
