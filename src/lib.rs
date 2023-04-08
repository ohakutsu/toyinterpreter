pub mod ast;
pub mod evaluate;
pub mod lexer;
pub mod parse;
pub mod repl;
pub mod token;

use evaluate::Evaluator;
use lexer::Lexer;
use parse::Parser;

pub fn exec(input: &str) {
    let mut evaluator = Evaluator::new();

    let tokens = Lexer::new(input).collect();
    let nodes = Parser::new(tokens).parse();

    for node in nodes {
        evaluator.eval(node);
    }
}
