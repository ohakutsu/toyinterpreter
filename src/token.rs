#[derive(Debug, PartialEq)]
pub enum Token {
    Num(i32),
    Ident(String),   // Identifiers
    Punct(String),   // Punctuators
    Keyword(String), // Keywords
}

pub struct Tokenizer {
    input: String,
    position: usize,
}

impl Tokenizer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.to_string(),
            position: 0,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.position)
    }

    fn pop_char(&mut self) -> Option<char> {
        let c = self.peek_char();
        self.position += 1;
        c
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.peek_char() {
                None => return None,
                Some(c) => {
                    if is_white_space(c) {
                        self.pop_char();
                        continue;
                    }
                    if c == '\0' {
                        return None;
                    }

                    if is_alphabet(c) {
                        self.pop_char();

                        let mut ident = c.to_string();
                        while let Some(c) = self.peek_char() {
                            if is_alphabet(c) || is_number(c) {
                                self.pop_char();
                                ident.push(c);
                            } else {
                                break;
                            }
                        }

                        if is_keyword(&ident) {
                            return Some(Token::Keyword(ident));
                        }

                        return Some(Token::Ident(ident));
                    }

                    if is_number(c) {
                        self.pop_char();

                        let mut num = c.to_string();
                        while let Some(c) = self.peek_char() {
                            if is_number(c) {
                                self.pop_char();
                                num.push(c);
                            } else {
                                break;
                            }
                        }

                        let num = num.parse().unwrap();
                        return Some(Token::Num(num));
                    }

                    if is_punct(c) {
                        self.pop_char();

                        if let Some(c2) = self.peek_char() {
                            let puncts = [c, c2].map(|c| c.to_string()).join("");
                            if ["==", "!=", ">=", "<="].contains(&puncts.as_str()) {
                                self.pop_char();
                                return Some(Token::Punct(puncts));
                            }
                        }

                        return Some(Token::Punct(c.to_string()));
                    }

                    panic!("unexpected char: {}", c);
                }
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
fn is_keyword(s: &str) -> bool {
    ["print", "exit", "time", "if", "for"].contains(&s)
}

#[cfg(test)]
mod tests {
    use crate::token::*;

    fn assert_tokenize(input: &str, expect: Vec<Token>) {
        let tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.collect::<Vec<_>>();

        assert_eq!(tokens, expect);
    }

    #[test]
    fn test_tokenizer() {
        assert_tokenize(
            "abc = 123;\
             def = 456;\
             ans = abc + def;\
             print ans;\
             time;\
             exit;",
            vec![
                Token::Ident("abc".to_string()),
                Token::Punct("=".to_string()),
                Token::Num(123),
                Token::Punct(";".to_string()),
                Token::Ident("def".to_string()),
                Token::Punct("=".to_string()),
                Token::Num(456),
                Token::Punct(";".to_string()),
                Token::Ident("ans".to_string()),
                Token::Punct("=".to_string()),
                Token::Ident("abc".to_string()),
                Token::Punct("+".to_string()),
                Token::Ident("def".to_string()),
                Token::Punct(";".to_string()),
                Token::Keyword("print".to_string()),
                Token::Ident("ans".to_string()),
                Token::Punct(";".to_string()),
                Token::Keyword("time".to_string()),
                Token::Punct(";".to_string()),
                Token::Keyword("exit".to_string()),
                Token::Punct(";".to_string()),
            ],
        );

        assert_tokenize(
            "i = 0;\
             if (i <= 10) i = i + 10;\
             time;",
            vec![
                Token::Ident("i".to_string()),
                Token::Punct("=".to_string()),
                Token::Num(0),
                Token::Punct(";".to_string()),
                Token::Keyword("if".to_string()),
                Token::Punct("(".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("<=".to_string()),
                Token::Num(10),
                Token::Punct(")".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("=".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("+".to_string()),
                Token::Num(10),
                Token::Punct(";".to_string()),
                Token::Keyword("time".to_string()),
                Token::Punct(";".to_string()),
            ],
        );

        assert_tokenize(
            "i = 0;\
             for (i = 0; i < 10; i = i + 1) print i;",
            vec![
                Token::Ident("i".to_string()),
                Token::Punct("=".to_string()),
                Token::Num(0),
                Token::Punct(";".to_string()),
                Token::Keyword("for".to_string()),
                Token::Punct("(".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("=".to_string()),
                Token::Num(0),
                Token::Punct(";".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("<".to_string()),
                Token::Num(10),
                Token::Punct(";".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("=".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct("+".to_string()),
                Token::Num(1),
                Token::Punct(")".to_string()),
                Token::Keyword("print".to_string()),
                Token::Ident("i".to_string()),
                Token::Punct(";".to_string()),
            ],
        )
    }
}
