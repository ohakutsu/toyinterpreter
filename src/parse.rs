use crate::token::Token;
use std::{iter::Peekable, vec::IntoIter};

#[derive(Debug, PartialEq)]
pub enum Node {
    Num(i32),                     // number
    Add(Box<Node>, Box<Node>),    // "+"
    Sub(Box<Node>, Box<Node>),    // "-"
    Assign(Box<Node>, Box<Node>), // "="
    LVar(String),                 // local variable
    Eq(Box<Node>, Box<Node>),     // "=="
    Ne(Box<Node>, Box<Node>),     // "!="
    LE(Box<Node>, Box<Node>),     // "<="
    GE(Box<Node>, Box<Node>),     // ">=",
    LT(Box<Node>, Box<Node>),     // "<"
    GT(Box<Node>, Box<Node>),     // ">"
    If(Box<Node>, Box<Node>),     // "if" "(" cond ")" then
    Print(Box<Node>),             // "print"
}

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let tokens = tokens.into_iter().peekable();

        Self { tokens }
    }

    pub fn parse(&mut self) -> Vec<Node> {
        let mut nodes = Vec::new();
        while self.tokens.peek().is_some() {
            let node = self.stmt();
            nodes.push(node);
        }
        nodes
    }

    // stmt = "if" "(" expr ")" stmt
    //      | "print" expr ";"
    //      | expr ";"
    fn stmt(&mut self) -> Node {
        if self.consume_token(Token::Keyword("if".to_string())) {
            self.expect_token(Token::Punct("(".to_string()));
            let cond = self.expr();
            self.expect_token(Token::Punct(")".to_string()));
            let then = self.stmt();
            return Node::If(Box::new(cond), Box::new(then));
        }

        if self.consume_token(Token::Keyword("print".to_string())) {
            let val = self.expr();
            self.expect_token(Token::Punct(";".to_string()));
            return Node::Print(Box::new(val));
        }

        let node = self.expr();
        self.expect_token(Token::Punct(";".to_string()));
        node
    }

    // expr = assign
    fn expr(&mut self) -> Node {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> Node {
        let mut node = self.equality();
        if self.consume_token(Token::Punct("=".to_string())) {
            node = Node::Assign(Box::new(node), Box::new(self.assign()));
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Node {
        let mut node = self.relational();
        loop {
            if self.consume_token(Token::Punct("==".to_string())) {
                node = Node::Eq(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::Punct("!=".to_string())) {
                node = Node::Ne(Box::new(node), Box::new(self.add()));
            } else {
                break;
            }
        }
        node
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Node {
        let mut node = self.add();
        loop {
            if self.consume_token(Token::Punct("<".to_string())) {
                node = Node::LT(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::Punct("<=".to_string())) {
                node = Node::LE(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::Punct(">".to_string())) {
                node = Node::GT(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::Punct(">=".to_string())) {
                node = Node::GE(Box::new(node), Box::new(self.add()));
            } else {
                break;
            }
        }
        node
    }

    // add = primary ("+" primary | "-" primary)*
    fn add(&mut self) -> Node {
        let mut node = self.primary();
        loop {
            if self.consume_token(Token::Punct("+".to_string())) {
                node = Node::Add(Box::new(node), Box::new(self.primary()));
            } else if self.consume_token(Token::Punct("-".to_string())) {
                node = Node::Sub(Box::new(node), Box::new(self.primary()));
            } else {
                break;
            }
        }
        node
    }

    // primary = num | ident
    fn primary(&mut self) -> Node {
        let token = self.tokens.next();
        match token {
            Some(Token::Num(n)) => Node::Num(n),
            Some(Token::Ident(name)) => Node::LVar(name),
            _ => panic!("unexpected token: {:?}", token),
        }
    }

    fn consume_token(&mut self, token: Token) -> bool {
        if self.tokens.next_if_eq(&token).is_some() {
            return true;
        }

        false
    }

    fn expect_token(&mut self, token: Token) {
        if self.tokens.next_if_eq(&token).is_none() {
            panic!("unexpected token: {:?}", token);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::*;
    use crate::token::Tokenizer;

    fn assert_nodes(input: &str, expect: Vec<Node>) {
        let tokens = Tokenizer::new(input).collect::<Vec<_>>();
        let mut parser = Parser::new(tokens);
        let nodes = parser.parse();
        assert_eq!(nodes, expect);
    }

    #[test]
    fn test_parser() {
        assert_nodes(
            "abc = 123;\
             def = 456;\
             ans = abc + def;",
            vec![
                Node::Assign(
                    Box::new(Node::LVar("abc".to_string())),
                    Box::new(Node::Num(123)),
                ),
                Node::Assign(
                    Box::new(Node::LVar("def".to_string())),
                    Box::new(Node::Num(456)),
                ),
                Node::Assign(
                    Box::new(Node::LVar("ans".to_string())),
                    Box::new(Node::Add(
                        Box::new(Node::LVar("abc".to_string())),
                        Box::new(Node::LVar("def".to_string())),
                    )),
                ),
            ],
        );

        assert_nodes(
            "i = 0;\
             if (i <= 10) i = i + 10;",
            vec![
                Node::Assign(
                    Box::new(Node::LVar("i".to_string())),
                    Box::new(Node::Num(0)),
                ),
                Node::If(
                    Box::new(Node::LE(
                        Box::new(Node::LVar("i".to_string())),
                        Box::new(Node::Num(10)),
                    )),
                    Box::new(Node::Assign(
                        Box::new(Node::LVar("i".to_string())),
                        Box::new(Node::Add(
                            Box::new(Node::LVar("i".to_string())),
                            Box::new(Node::Num(10)),
                        )),
                    )),
                ),
            ],
        );

        assert_nodes(
            "i = 2;\
             print i;",
            vec![
                Node::Assign(
                    Box::new(Node::LVar("i".to_string())),
                    Box::new(Node::Num(2)),
                ),
                Node::Print(Box::new(Node::LVar("i".to_string()))),
            ],
        );
    }
}
