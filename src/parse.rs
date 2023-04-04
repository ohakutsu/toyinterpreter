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
    For(
        Option<Box<Node>>,
        Option<Box<Node>>,
        Option<Box<Node>>,
        Box<Node>,
    ), // "for" "(" init ";" cond ";" inc ")" then
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
    //      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    //      | "print" expr ";"
    //      | expr ";"
    fn stmt(&mut self) -> Node {
        if self.consume_token(Token::KeywordIf) {
            self.expect_token(Token::LParen);
            let cond = self.expr();
            self.expect_token(Token::RParen);
            let then = self.stmt();
            return Node::If(Box::new(cond), Box::new(then));
        }

        if self.consume_token(Token::KeywordFor) {
            let mut init = None;
            let mut cond = None;
            let mut inc = None;
            self.expect_token(Token::LParen);

            if !self.consume_token(Token::SemiColon) {
                init = Some(Box::new(self.expr()));
                self.expect_token(Token::SemiColon);
            }
            if !self.consume_token(Token::SemiColon) {
                cond = Some(Box::new(self.expr()));
                self.expect_token(Token::SemiColon);
            }
            if !self.consume_token(Token::RParen) {
                inc = Some(Box::new(self.expr()));
                self.expect_token(Token::RParen);
            }

            let then = Box::new(self.stmt());
            return Node::For(init, cond, inc, then);
        }

        if self.consume_token(Token::KeywordPrint) {
            let val = self.expr();
            self.expect_token(Token::SemiColon);
            return Node::Print(Box::new(val));
        }

        let node = self.expr();
        self.expect_token(Token::SemiColon);
        node
    }

    // expr = assign
    fn expr(&mut self) -> Node {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> Node {
        let mut node = self.equality();
        if self.consume_token(Token::Assign) {
            node = Node::Assign(Box::new(node), Box::new(self.assign()));
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Node {
        let mut node = self.relational();
        loop {
            if self.consume_token(Token::Equal) {
                node = Node::Eq(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::NotEqual) {
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
            if self.consume_token(Token::LessThan) {
                node = Node::LT(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::LessThanEqual) {
                node = Node::LE(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::GreaterThan) {
                node = Node::GT(Box::new(node), Box::new(self.add()));
            } else if self.consume_token(Token::GreaterThanEqual) {
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
            if self.consume_token(Token::Plus) {
                node = Node::Add(Box::new(node), Box::new(self.primary()));
            } else if self.consume_token(Token::Minus) {
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
            Some(Token::Int(n)) => Node::Num(n),
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
    use crate::lexer::Lexer;
    use crate::parse::*;

    fn assert_nodes(input: &str, expect: Vec<Node>) {
        let tokens = Lexer::new(input).collect::<Vec<_>>();
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

        assert_nodes(
            "i = 0;\
             for (i = 0; i < 10; i = i + 1) print i;",
            vec![
                Node::Assign(
                    Box::new(Node::LVar("i".to_string())),
                    Box::new(Node::Num(0)),
                ),
                Node::For(
                    Some(Box::new(Node::Assign(
                        Box::new(Node::LVar("i".to_string())),
                        Box::new(Node::Num(0)),
                    ))),
                    Some(Box::new(Node::LT(
                        Box::new(Node::LVar("i".to_string())),
                        Box::new(Node::Num(10)),
                    ))),
                    Some(Box::new(Node::Assign(
                        Box::new(Node::LVar("i".to_string())),
                        Box::new(Node::Add(
                            Box::new(Node::LVar("i".to_string())),
                            Box::new(Node::Num(1)),
                        )),
                    ))),
                    Box::new(Node::Print(Box::new(Node::LVar("i".to_string())))),
                ),
            ],
        );

        assert_nodes(
            "i = 0;\
             for (;;) print i;",
            vec![
                Node::Assign(
                    Box::new(Node::LVar("i".to_string())),
                    Box::new(Node::Num(0)),
                ),
                Node::For(
                    None,
                    None,
                    None,
                    Box::new(Node::Print(Box::new(Node::LVar("i".to_string())))),
                ),
            ],
        );
    }
}
