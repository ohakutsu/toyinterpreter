use crate::ast::{Expression, Infix, Literal, Statement};
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

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();
        while self.tokens.peek().is_some() {
            let node = self.stmt();
            statements.push(node);
        }
        statements
    }

    // stmt = "if" "(" expr ")" stmt
    //      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    //      | "print" expr ";"
    //      | expr ";"
    fn stmt(&mut self) -> Statement {
        if self.consume_token(Token::KeywordIf) {
            self.expect_token(Token::LParen);
            let cond = Box::new(self.expr());
            self.expect_token(Token::RParen);
            let then = Box::new(self.stmt());
            return Statement::If { cond, then };
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
            return Statement::For {
                init,
                cond,
                inc,
                then,
            };
        }

        if self.consume_token(Token::KeywordPrint) {
            let val = self.expr();
            self.expect_token(Token::SemiColon);
            return Statement::Print(val);
        }

        let node = self.expr();
        self.expect_token(Token::SemiColon);
        Statement::Expression(node)
    }

    // expr = assign
    fn expr(&mut self) -> Expression {
        self.assign()
    }

    // assign = equality ("=" assign)?
    fn assign(&mut self) -> Expression {
        let mut node = self.equality();
        if self.consume_token(Token::Assign) {
            node = Expression::Infix {
                op: Infix::Assign,
                lhs: Box::new(node),
                rhs: Box::new(self.assign()),
            };
        }
        node
    }

    // equality = relational ("==" relational | "!=" relational)*
    fn equality(&mut self) -> Expression {
        let mut node = self.relational();
        loop {
            if self.consume_token(Token::Equal) {
                node = Expression::Infix {
                    op: Infix::Equal,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                };
            } else if self.consume_token(Token::NotEqual) {
                node = Expression::Infix {
                    op: Infix::NotEqual,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                };
            } else {
                break;
            }
        }
        node
    }

    // relational = add ("<" add | "<=" add | ">" add | ">=" add)*
    fn relational(&mut self) -> Expression {
        let mut node = self.add();
        loop {
            if self.consume_token(Token::LessThan) {
                node = Expression::Infix {
                    op: Infix::LessThan,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                };
            } else if self.consume_token(Token::LessThanEqual) {
                node = Expression::Infix {
                    op: Infix::LessThanEqual,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                };
            } else if self.consume_token(Token::GreaterThan) {
                node = Expression::Infix {
                    op: Infix::GreaterThan,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                };
            } else if self.consume_token(Token::GreaterThanEqual) {
                node = Expression::Infix {
                    op: Infix::GreaterThanEqual,
                    lhs: Box::new(node),
                    rhs: Box::new(self.add()),
                };
            } else {
                break;
            }
        }
        node
    }

    // add = primary ("+" primary | "-" primary)*
    fn add(&mut self) -> Expression {
        let mut node = self.primary();
        loop {
            if self.consume_token(Token::Plus) {
                node = Expression::Infix {
                    op: Infix::Plus,
                    lhs: Box::new(node),
                    rhs: Box::new(self.primary()),
                };
            } else if self.consume_token(Token::Minus) {
                node = Expression::Infix {
                    op: Infix::Minus,
                    lhs: Box::new(node),
                    rhs: Box::new(self.primary()),
                };
            } else {
                break;
            }
        }
        node
    }

    // primary = num | ident
    fn primary(&mut self) -> Expression {
        let token = self.tokens.next();
        match token {
            Some(Token::Int(n)) => Expression::Literal(Literal::Int(n)),
            Some(Token::Ident(name)) => Expression::Ident(name),
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
    use crate::ast::*;
    use crate::lexer::Lexer;
    use crate::parse::*;

    fn assert_nodes(input: &str, expect: Vec<Statement>) {
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
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("abc".to_string())),
                    rhs: Box::new(Expression::Literal(Literal::Int(123))),
                }),
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("def".to_string())),
                    rhs: Box::new(Expression::Literal(Literal::Int(456))),
                }),
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("ans".to_string())),
                    rhs: Box::new(Expression::Infix {
                        op: Infix::Plus,
                        lhs: Box::new(Expression::Ident("abc".to_string())),
                        rhs: Box::new(Expression::Ident("def".to_string())),
                    }),
                }),
            ],
        );

        assert_nodes(
            "i = 0;\
             if (i <= 10) i = i + 10;",
            vec![
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("i".to_string())),
                    rhs: Box::new(Expression::Literal(Literal::Int(0))),
                }),
                Statement::If {
                    cond: Box::new(Expression::Infix {
                        op: Infix::LessThanEqual,
                        lhs: Box::new(Expression::Ident("i".to_string())),
                        rhs: Box::new(Expression::Literal(Literal::Int(10))),
                    }),
                    then: Box::new(Statement::Expression(Expression::Infix {
                        op: Infix::Assign,
                        lhs: Box::new(Expression::Ident("i".to_string())),
                        rhs: Box::new(Expression::Infix {
                            op: Infix::Plus,
                            lhs: Box::new(Expression::Ident("i".to_string())),
                            rhs: Box::new(Expression::Literal(Literal::Int(10))),
                        }),
                    })),
                },
            ],
        );

        assert_nodes(
            "i = 2;\
             print i;",
            vec![
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("i".to_string())),
                    rhs: Box::new(Expression::Literal(Literal::Int(2))),
                }),
                Statement::Print(Expression::Ident("i".to_string())),
            ],
        );

        assert_nodes(
            "i = 0;\
             for (i = 0; i < 10; i = i + 1) print i;",
            vec![
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("i".to_string())),
                    rhs: Box::new(Expression::Literal(Literal::Int(0))),
                }),
                Statement::For {
                    init: Some(Box::new(Expression::Infix {
                        op: Infix::Assign,
                        lhs: Box::new(Expression::Ident("i".to_string())),
                        rhs: Box::new(Expression::Literal(Literal::Int(0))),
                    })),
                    cond: Some(Box::new(Expression::Infix {
                        op: Infix::LessThan,
                        lhs: Box::new(Expression::Ident("i".to_string())),
                        rhs: Box::new(Expression::Literal(Literal::Int(10))),
                    })),
                    inc: Some(Box::new(Expression::Infix {
                        op: Infix::Assign,
                        lhs: Box::new(Expression::Ident("i".to_string())),
                        rhs: Box::new(Expression::Infix {
                            op: Infix::Plus,
                            lhs: Box::new(Expression::Ident("i".to_string())),
                            rhs: Box::new(Expression::Literal(Literal::Int(1))),
                        }),
                    })),
                    then: Box::new(Statement::Print(Expression::Ident("i".to_string()))),
                },
            ],
        );

        assert_nodes(
            "i = 0;\
             for (;;) print i;",
            vec![
                Statement::Expression(Expression::Infix {
                    op: Infix::Assign,
                    lhs: Box::new(Expression::Ident("i".to_string())),
                    rhs: Box::new(Expression::Literal(Literal::Int(0))),
                }),
                Statement::For {
                    init: None,
                    cond: None,
                    inc: None,
                    then: Box::new(Statement::Print(Expression::Ident("i".to_string()))),
                },
            ],
        );
    }
}
