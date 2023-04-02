use std::{env, fs};

fn main() -> Result<(), ()> {
    let args = env::args().collect::<Vec<String>>();
    let argc = args.len();

    if argc >= 2 {
        let program = fs::read_to_string(&args[1]).unwrap();
        toyinterpreter::exec(&program);
    } else {
        toyinterpreter::repl::start();
    }

    Ok(())
}
