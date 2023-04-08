use crate::evaluate::Evaluator;
use crate::lexer::Lexer;
use crate::parse::Parser;
use std::io::{self, Write};

pub fn start() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();
    let mut evaluator = Evaluator::new();

    loop {
        print!("> ");
        stdout.flush().unwrap();
        input.clear();
        stdin.read_line(&mut input).unwrap();

        if input.trim() == "exit" {
            break;
        }

        input.push(';');
        let tokens = Lexer::new(input.trim()).collect();
        let nodes = Parser::new(tokens).parse();

        for node in nodes {
            if let Some(obj) = evaluator.eval(node) {
                println!("{}", obj);
            }
        }
    }
}

#[cfg(test)]
mod tests {}
