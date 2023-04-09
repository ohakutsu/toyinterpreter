use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.peek_char();
        self.pos += 1;
        c
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(c) = self.peek_char() {
                if is_white_space(c) {
                    self.next_char();
                    continue;
                }
            }

            break;
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();

        match self.peek_char() {
            None => None,
            Some(c) => {
                if is_alphabet(c) {
                    self.next_char();

                    let mut ident = c.to_string();
                    while let Some(c) = self.peek_char() {
                        if is_alphabet(c) || is_number(c) {
                            self.next_char();
                            ident.push(c);
                        } else {
                            break;
                        }
                    }

                    let token = match ident.as_str() {
                        "exit" => Token::KeywordExit,
                        "for" => Token::KeywordFor,
                        "if" => Token::KeywordIf,
                        "print" => Token::KeywordPrint,
                        "time" => Token::KeywordTime,
                        _ => Token::Ident(ident),
                    };

                    return Some(token);
                }

                if is_number(c) {
                    self.next_char();

                    let mut num = c.to_string();
                    while let Some(c) = self.peek_char() {
                        if is_number(c) {
                            self.next_char();
                            num.push(c);
                        } else {
                            break;
                        }
                    }

                    let num = num.parse().unwrap();
                    return Some(Token::Int(num));
                }

                if is_punct(c) {
                    self.next_char();

                    if let Some(c2) = self.peek_char() {
                        let puncts = format!("{}{}", c, c2);

                        let token = match puncts.as_str() {
                            "==" => Some(Token::Equal),
                            "!=" => Some(Token::NotEqual),
                            ">=" => Some(Token::GreaterThanEqual),
                            "<=" => Some(Token::LessThanEqual),
                            _ => None,
                        };
                        if token.is_some() {
                            self.next_char();
                            return token;
                        }
                    }

                    let token = match &c {
                        '+' => Token::Plus,
                        '-' => Token::Minus,
                        '*' => Token::Asterisk,
                        '/' => Token::Slash,
                        '=' => Token::Assign,
                        '>' => Token::GreaterThan,
                        '<' => Token::LessThan,
                        '(' => Token::LParen,
                        ')' => Token::RParen,
                        ';' => Token::SemiColon,
                        _ => panic!("unexpected punct: {}", c),
                    };

                    return Some(token);
                }

                panic!("unexpected char: {}", c);
            }
        }
    }
}

fn is_white_space(c: char) -> bool {
    [' ', '\t', '\n', '\r'].contains(&c)
}
fn is_punct(c: char) -> bool {
    "!#%&()*+,-./:;<=>?[]{|}~".contains(c)
}
fn is_alphabet(c: char) -> bool {
    if ('a'..='z').contains(&c) {
        return true;
    }
    if ('A'..='Z').contains(&c) {
        return true;
    }
    if c == '_' {
        return true;
    }

    false
}
fn is_number(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn test_skip_whitespace() {
        let input = "  abc = 1;";
        let mut lexer = Lexer::new(input);
        lexer.skip_whitespace();
        assert_eq!(Some('a'), lexer.next_char());

        let input = "def = 2;";
        let mut lexer = Lexer::new(input);

        lexer.skip_whitespace();
        assert_eq!(Some('d'), lexer.next_char());
    }

    fn assert_tokens(input: &str, expect: Vec<Token>) {
        let lexer = Lexer::new(input);
        let tokens = lexer.collect::<Vec<_>>();

        assert_eq!(tokens, expect);
    }

    #[test]
    fn test_lexer() {
        assert_tokens(
            "abc = 123;\
             def = 456;\
             ans = abc + def;\
             print ans;\
             time;\
             exit;",
            vec![
                Token::Ident("abc".to_string()),
                Token::Assign,
                Token::Int(123),
                Token::SemiColon,
                Token::Ident("def".to_string()),
                Token::Assign,
                Token::Int(456),
                Token::SemiColon,
                Token::Ident("ans".to_string()),
                Token::Assign,
                Token::Ident("abc".to_string()),
                Token::Plus,
                Token::Ident("def".to_string()),
                Token::SemiColon,
                Token::KeywordPrint,
                Token::Ident("ans".to_string()),
                Token::SemiColon,
                Token::KeywordTime,
                Token::SemiColon,
                Token::KeywordExit,
                Token::SemiColon,
            ],
        );

        assert_tokens(
            "i = 0;\
             if (i <= 10) i = i + 10;\
             time;",
            vec![
                Token::Ident("i".to_string()),
                Token::Assign,
                Token::Int(0),
                Token::SemiColon,
                Token::KeywordIf,
                Token::LParen,
                Token::Ident("i".to_string()),
                Token::LessThanEqual,
                Token::Int(10),
                Token::RParen,
                Token::Ident("i".to_string()),
                Token::Assign,
                Token::Ident("i".to_string()),
                Token::Plus,
                Token::Int(10),
                Token::SemiColon,
                Token::KeywordTime,
                Token::SemiColon,
            ],
        );

        assert_tokens(
            "i = 0;\
             for (i = 0; i < 10; i = i + 1) print i;",
            vec![
                Token::Ident("i".to_string()),
                Token::Assign,
                Token::Int(0),
                Token::SemiColon,
                Token::KeywordFor,
                Token::LParen,
                Token::Ident("i".to_string()),
                Token::Assign,
                Token::Int(0),
                Token::SemiColon,
                Token::Ident("i".to_string()),
                Token::LessThan,
                Token::Int(10),
                Token::SemiColon,
                Token::Ident("i".to_string()),
                Token::Assign,
                Token::Ident("i".to_string()),
                Token::Plus,
                Token::Int(1),
                Token::RParen,
                Token::KeywordPrint,
                Token::Ident("i".to_string()),
                Token::SemiColon,
            ],
        );

        assert_tokens(
            "a = 2 * 3;\
             b = 4 / 2;",
            vec![
                Token::Ident("a".to_string()),
                Token::Assign,
                Token::Int(2),
                Token::Asterisk,
                Token::Int(3),
                Token::SemiColon,
                Token::Ident("b".to_string()),
                Token::Assign,
                Token::Int(4),
                Token::Slash,
                Token::Int(2),
                Token::SemiColon,
            ],
        );
    }
}
