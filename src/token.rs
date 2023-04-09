#[derive(Debug, PartialEq)]
pub enum Token {
    // Identifiers, Literals
    Ident(String),
    Int(i32),

    // Operators
    Plus,             // "+"
    Minus,            // "-"
    Asterisk,         // "*"
    Slash,            //"/"
    Assign,           // "="
    Equal,            // "=="
    NotEqual,         // "!="
    GreaterThan,      // ">"
    GreaterThanEqual, // ">="
    LessThan,         // "<"
    LessThanEqual,    // "<="

    // Keywords
    KeywordExit,  // "exit"
    KeywordFor,   //"for"
    KeywordIf,    // "if"
    KeywordPrint, // "print"
    KeywordTime,  // "time"

    // Delimiters
    LParen,    // "("
    RParen,    // ")"
    SemiColon, // ";"
}
