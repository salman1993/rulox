use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::ops::{Index, IndexMut};
use std::slice::SliceIndex;

use crate::reader::Source;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,
    // One or more two character tokens
    Bang,
    Equal,
    Less,
    Greater,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    // Literals
    String,
    Number,
    Identifier,
    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    EOF,
}

// Define a static HashMap of keywords
pub static KEYWORD_MAP: Lazy<HashMap<&'static str, TokenType>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert("and", TokenType::And);
    m.insert("class", TokenType::Class);
    m.insert("else", TokenType::Else);
    m.insert("false", TokenType::False);
    m.insert("for", TokenType::For);
    m.insert("fun", TokenType::Fun);
    m.insert("if", TokenType::If);
    m.insert("nil", TokenType::Nil);
    m.insert("or", TokenType::Or);
    m.insert("print", TokenType::Print);
    m.insert("return", TokenType::Return);
    m.insert("super", TokenType::Super);
    m.insert("this", TokenType::This);
    m.insert("true", TokenType::True);
    m.insert("var", TokenType::Var);
    m.insert("while", TokenType::While);

    m
});

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub toktype: TokenType,
    pub lexeme: String, // actual raw text string - "10_000"
    pub line: usize,
}

impl Token {
    pub fn new(toktype: TokenType, lexeme: &str, line: usize) -> Token {
        Token {
            toktype: toktype,
            lexeme: lexeme.to_string(),
            line: line,
        }
    }

    // This is mostly used as a helper for writing tests
    pub fn from_toktype(toktype: TokenType) -> Token {
        match toktype {
            TokenType::Plus => Token::new(TokenType::Plus, "+", 1),
            TokenType::Minus => Token::new(TokenType::Minus, "-", 1),
            TokenType::Star => Token::new(TokenType::Star, "*", 1),
            TokenType::Slash => Token::new(TokenType::Slash, "/", 1),
            TokenType::LeftParen => Token::new(TokenType::LeftParen, "(", 1),
            TokenType::RightParen => Token::new(TokenType::RightParen, ")", 1),
            TokenType::Greater => Token::new(TokenType::Greater, ">", 1),
            TokenType::GreaterEqual => Token::new(TokenType::GreaterEqual, ">=", 1),
            TokenType::Less => Token::new(TokenType::Less, "<", 1),
            TokenType::LessEqual => Token::new(TokenType::LessEqual, "<=", 1),
            TokenType::Or => Token::new(TokenType::Or, "or", 1),
            TokenType::And => Token::new(TokenType::And, "and", 1),
            _ => panic!("Token type not supported"),
        }
    }

    pub fn with_line(mut self, line: usize) -> Self {
        self.line = line;
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tokens {
    pub tokens: Vec<Token>,
}

impl Tokens {
    pub fn new(tokens: Vec<Token>) -> Tokens {
        Tokens { tokens }
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn get<I>(&self, index: I) -> Option<&I::Output>
    where
        I: SliceIndex<[Token]>,
    {
        self.tokens.get(index)
    }
}

// Index Trait: Implements the Index trait to allow immutable indexing.
impl Index<usize> for Tokens {
    type Output = Token;

    fn index(&self, index: usize) -> &Self::Output {
        &self.tokens[index]
    }
}

// IndexMut Trait: Implements the IndexMut trait to allow mutable indexing.
impl IndexMut<usize> for Tokens {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.tokens[index]
    }
}

pub fn tokenize(source: Source) -> Result<Tokens, String> {
    let mut tokens = Vec::new();
    let mut char_indices = source.content.char_indices().peekable();
    let mut line: usize = 1;

    while let Some((start, ch)) = char_indices.next() {
        // println!("\n\nProcessing at index '{start}', char: '{ch}'");
        match ch {
            // Whitespace (ignore)
            ' ' | '\t' | '\r' => {}
            '\n' => {
                line += 1;
            }
            // Single-character tokens.
            '(' => tokens.push(Token::new(TokenType::LeftParen, "(", line)),
            ')' => tokens.push(Token::new(TokenType::RightParen, ")", line)),
            '{' => tokens.push(Token::new(TokenType::LeftBrace, "{", line)),
            '}' => tokens.push(Token::new(TokenType::RightBrace, "}", line)),
            ',' => tokens.push(Token::new(TokenType::Comma, ",", line)),
            '.' => tokens.push(Token::new(TokenType::Dot, ".", line)),
            '-' => tokens.push(Token::new(TokenType::Minus, "-", line)),
            '+' => tokens.push(Token::new(TokenType::Plus, "+", line)),
            ';' => tokens.push(Token::new(TokenType::Semicolon, ";", line)),
            '*' => tokens.push(Token::new(TokenType::Star, "*", line)),
            // One or more two character tokens
            '!' => {
                if let Some((_, '=')) = char_indices.peek() {
                    char_indices.next();
                    tokens.push(Token::new(TokenType::BangEqual, "!=", line))
                } else {
                    tokens.push(Token::new(TokenType::Bang, "!", line))
                }
            }
            '=' => {
                if let Some((_, '=')) = char_indices.peek() {
                    char_indices.next();
                    tokens.push(Token::new(TokenType::EqualEqual, "==", line))
                } else {
                    tokens.push(Token::new(TokenType::Equal, "=", line))
                }
            }
            '<' => {
                if let Some((_, '=')) = char_indices.peek() {
                    char_indices.next();
                    tokens.push(Token::new(TokenType::LessEqual, "<=", line))
                } else {
                    tokens.push(Token::new(TokenType::Less, "<", line))
                }
            }
            '>' => {
                if let Some((_, '=')) = char_indices.peek() {
                    char_indices.next();
                    tokens.push(Token::new(TokenType::GreaterEqual, ">=", line))
                } else {
                    tokens.push(Token::new(TokenType::Greater, ">", line))
                }
            }
            // Slash -> can be a comment or division
            '/' => {
                if let Some((_, '/')) = char_indices.peek() {
                    // If its a comment, consume/skip chars till we hit next line
                    while let Some((_, comment_ch)) = char_indices.next() {
                        // println!("processing comment char: {comment_ch}");
                        if comment_ch == '\n' {
                            line += 1;
                            break;
                        }
                    }
                } else {
                    // Else, add slash for division operator
                    tokens.push(Token::new(TokenType::Slash, "/", line));
                }
            }
            // String literal
            '"' => {
                let mut string_found = false;
                while let Some((n, string_ch)) = char_indices.next() {
                    if string_ch == '"' {
                        string_found = true;
                        // trimmed the quotes from the text string
                        let text = &source.content[start + 1..n];
                        tokens.push(Token::new(TokenType::String, text, line));
                        break;
                    }
                }
                if !string_found {
                    return Err(format!("Error in line {line}: could not create string"));
                }
            }
            // Number literal
            _ if ch.is_digit(10) => {
                // println!("Number...");
                let mut end = start + 1;

                // Consume integer part
                while let Some((_, number_ch)) = char_indices.peek() {
                    if number_ch.is_digit(10) {
                        char_indices.next();
                        end += 1;
                    } else {
                        break;
                    }
                }

                // Check for fractional part
                if let Some((_, '.')) = char_indices.peek() {
                    if let Some((_, next_ch)) = char_indices.clone().nth(1) {
                        // Lookahead one more character
                        if next_ch.is_digit(10) {
                            // Consume the decimal point
                            char_indices.next();
                            end += 1;

                            // Consume fractional digits
                            while let Some((_, fraction_ch)) = char_indices.peek() {
                                if fraction_ch.is_digit(10) {
                                    char_indices.next();
                                    end += 1;
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                }

                let text = &source.content[start..end];
                tokens.push(Token::new(TokenType::Number, text, line));
            }
            // Identifier or a Keyword
            _ if ch.is_ascii_alphabetic() || (ch == '_') => {
                let mut end = start + 1;
                while let Some(&(n, identifier_ch)) = char_indices.peek() {
                    end = n;
                    if !identifier_ch.is_alphanumeric() {
                        break;
                    } else {
                        char_indices.next();
                        end = n + 1;
                    }
                }
                // Determine whether toktype is identifier or a keyword
                let text = &source.content[start..end];
                let toktype = KEYWORD_MAP.get(text).unwrap_or(&TokenType::Identifier);
                tokens.push(Token::new(toktype.clone(), text, line));
            }

            _ => {
                return Err(format!("Unexpected character at line {line}."));
            }
        }
    }

    // Add EOF token at the end
    tokens.push(Token::new(TokenType::EOF, "", line));
    Ok(Tokens { tokens })
}

#[cfg(test)]
mod tests {
    use crate::{reader::Source, tokenize::TokenType};

    use super::tokenize;

    #[test]
    fn test_tokenize_data_types_boolean() {
        let content = "true";
        let tokens = tokenize(Source::from_content(content)).unwrap();
        assert_eq!(tokens[0].toktype, TokenType::True);
        assert_eq!(tokens[0].lexeme, "true");
        assert_eq!(tokens.len(), 2); // True, EOF
    }

    #[test]
    fn test_tokenize_data_types_number() {
        let content = "\n\n12.34;";
        let tokens = tokenize(Source::from_content(content)).unwrap();
        assert_eq!(tokens.len(), 3); // Number, Semicolon, EOF
        assert_eq!(tokens[0].toktype, TokenType::Number);
        assert_eq!(tokens[0].lexeme, "12.34");
        assert_eq!(tokens[0].line, 3);
    }

    #[test]
    fn test_tokenize_data_types_string() {
        let content = "\"I am a string\"";
        let tokens = tokenize(Source::from_content(content)).unwrap();
        assert_eq!(tokens.len(), 2); // String, EOF
        assert_eq!(tokens[0].toktype, TokenType::String);
        assert_eq!(tokens[0].lexeme, "I am a string");
    }

    #[test]
    fn test_tokenize_variable_assignment() {
        let content = "var xyz = 2.13;";
        let tokens = tokenize(Source::from_content(content)).unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0].toktype, TokenType::Var);
        assert_eq!(tokens[1].toktype, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "xyz");
        assert_eq!(tokens[2].toktype, TokenType::Equal);
        assert_eq!(tokens[3].toktype, TokenType::Number);
        assert_eq!(tokens[3].lexeme, "2.13");
        assert_eq!(tokens[4].toktype, TokenType::Semicolon);
        assert_eq!(tokens[5].toktype, TokenType::EOF);
    }

    #[test]
    fn test_tokenize_control_flow() {
        let content = "if (1 == 1) {\n  print \"hey hey\";\nelse {\n  print \"hi there\";\n}";
        let tokens = tokenize(Source::from_content(content)).unwrap();

        assert_eq!(tokens[0].toktype, TokenType::If);
        assert_eq!(tokens[3].toktype, TokenType::EqualEqual);
        assert_eq!(tokens[7].toktype, TokenType::Print);
        assert_eq!(tokens[10].toktype, TokenType::Else);
        assert_eq!(tokens[10].line, 3);

        assert_eq!(tokens[13].toktype, TokenType::String);
        assert_eq!(tokens[13].lexeme, "hi there");
    }

    #[test]
    fn test_tokenize_functions() {
        let content = "fun printSum(foo, bar) {\n  print foo + bar;\n}";
        let tokens = tokenize(Source::from_content(content)).unwrap();

        assert_eq!(tokens[0].toktype, TokenType::Fun);
        assert_eq!(tokens[1].toktype, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "printSum");
        assert_eq!(tokens[2].toktype, TokenType::LeftParen);
        assert_eq!(tokens[3].toktype, TokenType::Identifier);
        assert_eq!(tokens[3].lexeme, "foo");
        assert_eq!(tokens[4].toktype, TokenType::Comma);
        assert_eq!(tokens[5].toktype, TokenType::Identifier);
        assert_eq!(tokens[5].lexeme, "bar");
        assert_eq!(tokens[6].toktype, TokenType::RightParen);
    }

    #[test]
    fn test_tokenize_classes() {
        let content = "class Breakfast {\n  cook() {\n    print \"Eggs a-fryin'!\";\n  }\n\n  serve(who) {\n    print \"Enjoy your breakfast, \" + who + \".\";\n  }\n}";
        let tokens = tokenize(Source::from_content(content)).unwrap();

        assert_eq!(tokens[0].toktype, TokenType::Class);
        assert_eq!(tokens[1].toktype, TokenType::Identifier);
        assert_eq!(tokens[1].lexeme, "Breakfast");
        assert_eq!(tokens[2].toktype, TokenType::LeftBrace);

        assert_eq!(tokens[3].toktype, TokenType::Identifier);
        assert_eq!(tokens[3].lexeme, "cook");
        assert_eq!(tokens[3].line, 2);

        assert_eq!(tokens[4].toktype, TokenType::LeftParen);
        assert_eq!(tokens[5].toktype, TokenType::RightParen);

        assert_eq!(tokens[11].toktype, TokenType::Identifier);
        assert_eq!(tokens[11].lexeme, "serve");
        assert_eq!(tokens[11].line, 6);
        assert_eq!(tokens[12].toktype, TokenType::LeftParen);
        assert_eq!(tokens[13].toktype, TokenType::Identifier);
        assert_eq!(tokens[13].lexeme, "who");
        assert_eq!(tokens[14].toktype, TokenType::RightParen);
    }

    #[test]
    fn test_tokenize_tricky_case() {
        let content = "2.xyz;";
        let tokens = tokenize(Source::from_content(content)).unwrap();
        println!("Tokens: {tokens:?}");
        assert_eq!(tokens.len(), 5); // Number, Dot, Identifier, Semicolon, EOF
        assert_eq!(tokens[0].toktype, TokenType::Number);
        assert_eq!(tokens[0].lexeme, "2");
        assert_eq!(tokens[1].toktype, TokenType::Dot);
        assert_eq!(tokens[2].toktype, TokenType::Identifier);
        assert_eq!(tokens[2].lexeme, "xyz");
    }

    #[test]
    fn test_tokenize_failure_invalid_char() {
        let content = "@$@*(#@;";
        let result = tokenize(Source::from_content(content));

        assert!(result.is_err());

        let error_message = result.unwrap_err().to_string().to_lowercase();
        assert!(error_message.contains("unexpected character"));
    }
}
