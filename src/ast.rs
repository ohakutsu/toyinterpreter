#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i32),
}

#[derive(Debug, PartialEq)]
pub enum Infix {
    Plus,             // "+"
    Minus,            // "-"
    Multiply,         // "*"
    Divide,           // "/"
    Assign,           // "="
    Equal,            // "=="
    NotEqual,         // "!="
    GreaterThan,      // ">"
    GreaterThanEqual, // ">="
    LessThan,         // "<"
    LessThanEqual,    // "<="
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    If {
        cond: Box<Expression>,
        then: Box<Statement>,
    },
    For {
        init: Option<Box<Expression>>,
        cond: Option<Box<Expression>>,
        inc: Option<Box<Expression>>,
        then: Box<Statement>,
    },
    Print(Expression),
    Block(Vec<Statement>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Literal(Literal),
    Infix {
        op: Infix,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
}
