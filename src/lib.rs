pub mod evaluate;
pub mod parse;
pub mod repl;
pub mod token;

use evaluate::Evaluator;
use parse::Parser;
use token::Tokenizer;

pub fn exec(input: &str) {
    let mut evaluator = Evaluator::new();

    let tokens = Tokenizer::new(input).collect();
    let nodes = Parser::new(tokens).parse();

    for node in nodes {
        evaluator.eval(node);
    }
}
