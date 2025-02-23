use crate::ast::{Expr, Parameters, Statement, Statements, AST};
use crate::tokenize::{Token, TokenType, Tokens};

pub struct Parser<'a> {
    tokens: &'a Tokens,
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &Tokens) -> Parser {
        Parser { tokens, pos: 0 }
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    pub fn is_at_end(&self) -> bool {
        if self.pos >= (self.tokens.len() - 1) || self.peek().toktype == TokenType::EOF {
            return true;
        }
        false
    }

    // Check if the current token matches an expected type, but only look at it
    pub fn check(&self, expected: TokenType) -> bool {
        if self.peek().toktype == expected {
            return true;
        }
        false
    }

    // Check if the current token matches an expected type and consume it if so
    fn matching(&mut self, expected: TokenType) -> bool {
        if self.check(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    // Just looks at the previously consumed token
    pub fn previous(&self) -> &Token {
        self.tokens
            .get(self.pos - 1)
            .expect("No token found at the current position")
    }

    // Consumes the token - returns the token, moves the index forward
    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.pos += 1;
        }
        self.previous()
    }

    fn consume(&mut self, toktype: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(toktype) {
            Ok(self.advance())
        } else {
            let token = self.peek();
            let error_msg = format!(
                "Error: {} on line {} for text '{}'",
                message, token.line, token.lexeme
            );
            Err(error_msg)
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expr, String> {
        let expr = self.parse_expression_term_or_operator()?;
        // println!("\n[parse_expression] expr: {expr:?}");

        if self.matching(TokenType::Equal) {
            let value = self.parse_expression()?;
            if let Expr::Variable { name } = expr {
                Ok(Expr::Assign {
                    name,
                    expr: Box::new(value),
                })
            } else {
                Err(String::from("Invalid assignment target"))
            }
        } else {
            Ok(expr)
        }
    }

    pub fn parse_expression_term_or_operator(&mut self) -> Result<Expr, String> {
        let term = self.parse_unary()?;
        // println!("[parse_expr] term: {term:?}");

        let expr = {
            if self.matching(TokenType::Plus)
                || self.matching(TokenType::Minus)
                || self.matching(TokenType::Star)
                || self.matching(TokenType::Slash)
                || self.matching(TokenType::Less)
                || self.matching(TokenType::LessEqual)
                || self.matching(TokenType::Greater)
                || self.matching(TokenType::GreaterEqual)
                || self.matching(TokenType::EqualEqual)
                || self.matching(TokenType::BangEqual)
            {
                let op = self.previous().clone();
                // println!("[parse_expr] binary op: {:?} left_term: {:?}", op, term);
                Expr::Binary {
                    left: Box::new(term),
                    op: op,
                    right: Box::new(self.parse_unary()?),
                }
            } else if self.matching(TokenType::Or) || self.matching(TokenType::And) {
                let op = self.previous().clone();
                // println!("[parse_expr] logical op: {:?} left_term: {:?}", op, term);
                Expr::Logical {
                    left: Box::new(term),
                    op: op,
                    right: Box::new(self.parse_unary()?),
                }
            } else {
                return Ok(term);
            }
        };

        Ok(expr)
    }

    // Parses a single term from the left-hand side
    // Parse a terminal term or unary operator
    // It has to be one of these: Unary -> "-" | "!"
    //            OR Primary ->  NUMBER | STRING | "true" | "false" | "nil" | "("
    fn parse_unary(&mut self) -> Result<Expr, String> {
        if self.matching(TokenType::Minus) || self.matching(TokenType::Bang) {
            // Unary operator -> either "-" or "!"
            Ok(Expr::Unary {
                op: self.previous().clone(),
                expr: Box::new(self.parse_unary()?),
            })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;
        // println!("\n[parse_call] expr: {expr:?}");

        loop {
            if self.matching(TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, String> {
        let mut arguments: Vec<Expr> = Vec::new();

        while !self.check(TokenType::RightParen) {
            let expr = self.parse_expression()?;
            arguments.push(expr);

            if arguments.len() >= 255 {
                return Err("Can't have more than 255 arguments.".to_string());
            }

            // Match comma if it's there
            self.matching(TokenType::Comma);
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments")?;
        Ok(Expr::Call {
            callee: Box::new(callee),
            paren: paren.clone(),
            arguments: arguments,
        })
    }

    // In the book, this is called "primary" expression
    // It has to be one of these: NUMBER | STRING | "true" | "false" | "nil" | "("
    fn parse_primary(&mut self) -> Result<Expr, String> {
        let expr = {
            if self.matching(TokenType::String) {
                Expr::TString(self.previous().lexeme.clone())
            } else if self.matching(TokenType::Number) {
                let token = self.previous();
                match token.lexeme.parse::<f64>() {
                    Ok(parsed) => Expr::TNumber(parsed),
                    Err(_) => {
                        return Err(format!(
                            "Error in line {}: Could not convert lexeme to a number: {}",
                            token.line, token.lexeme
                        ));
                    }
                }
            } else if self.matching(TokenType::True) {
                Expr::TBoolean(true)
            } else if self.matching(TokenType::False) {
                Expr::TBoolean(false)
            } else if self.matching(TokenType::Nil) {
                Expr::TNil
            } else if self.matching(TokenType::LeftParen) {
                let group_expr = self.parse_expression()?;
                // try to match the right paren
                self.consume(TokenType::RightParen, "Missing parentheses")?;
                Expr::Grouping {
                    expr: Box::new(group_expr),
                }
            } else if self.matching(TokenType::Identifier) {
                Expr::Variable {
                    name: self.previous().clone(),
                }
            } else {
                let token = self.peek();
                return Err(format!(
                    "Error: could not parse term in line {} for text '{}'",
                    token.line, token.lexeme
                ));
            }
        };

        Ok(expr)
    }

    fn parse_block_stmts(&mut self) -> Result<Statements, String> {
        // block statement { ... }
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() && !self.check(TokenType::RightBrace) {
            statements.push(self.parse_statement()?);
        }
        self.consume(TokenType::RightBrace, "Expect '}' at the end of block.")?;
        Ok(Statements::new(statements))
    }

    fn parse_if_stmt(&mut self) -> Result<Statement, String> {
        // ifStmt         → "if" "(" expression ")" then_statements
        //                 ( "else" else_statements )? ;
        let condition = self.parse_expression()?; // this should handle the "(" ... ")"

        self.consume(
            TokenType::LeftBrace,
            "Expect '{' at the start of the if block.",
        )?;
        let then_stmts = self.parse_block_stmts()?;

        let mut else_stmts: Option<Statements> = None;
        if self.matching(TokenType::Else) {
            self.consume(
                TokenType::LeftBrace,
                "Expect '{' at the start of the else block.",
            )?;
            else_stmts = Some(self.parse_block_stmts()?);
        }
        Ok(Statement::IfSt {
            condition,
            then_branch: then_stmts,
            else_branch: else_stmts,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Statement, String> {
        let condition = self.parse_expression()?; // this should handle the "(" ... ")"
        self.consume(
            TokenType::LeftBrace,
            "Expect '{' at the start of the if block.",
        )?;
        let body = self.parse_block_stmts()?;
        Ok(Statement::WhileSt { condition, body })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.matching(TokenType::Var) {
            // var statement: var foo; OR var foo = 1.2;
            let identifier = self
                .consume(TokenType::Identifier, "Expect variable name.")?
                .clone();
            let mut initializer: Option<Expr> = None;
            if self.matching(TokenType::Equal) {
                initializer = Some(self.parse_expression()?);
            }
            self.consume(
                TokenType::Semicolon,
                "Expect ';' after variable declaration.",
            )?;
            Ok(Statement::VarSt {
                name: identifier,
                expr: initializer,
            })
        } else if self.matching(TokenType::Fun) {
            return self.parse_func_decl_stmt();
        } else if self.matching(TokenType::If) {
            return self.parse_if_stmt();
        } else if self.matching(TokenType::While) {
            return self.parse_while_stmt();
        } else if self.matching(TokenType::LeftBrace) {
            let stmts = self.parse_block_stmts()?;
            Ok(Statement::BlockSt { body: stmts })
        } else if self.matching(TokenType::Print) {
            // print statement
            let expr = self.parse_expression()?;
            self.consume(TokenType::Semicolon, "Missing semicolon after print")?;
            Ok(Statement::PrintSt { expr: expr })
        } else if self.matching(TokenType::Return) {
            let expr = self.parse_expression()?;
            self.consume(
                TokenType::Semicolon,
                "Missing semicolon after return statement",
            )?;
            Ok(Statement::ReturnSt { expr: expr })
        } else {
            // expression statement
            let expr = self.parse_expression()?;
            self.consume(TokenType::Semicolon, "Missing semicolon after expression")?;
            Ok(Statement::ExprSt { expr: expr })
        }
    }

    fn parse_func_decl_stmt(&mut self) -> Result<Statement, String> {
        let func_name = self
            .consume(TokenType::Identifier, "Expect function name.")?
            .clone();
        // println!("\n[parse_func_decl_stmt] func_name: {func_name:?}");

        self.consume(TokenType::LeftParen, "Expect '(' for defining parameters.")?;
        let mut params: Parameters = Vec::new();
        while !self.check(TokenType::RightParen) {
            let param_name = self.consume(TokenType::Identifier, "Expect parameter name")?;
            // println!("[parse_func_decl_stmt] param_name: {param_name:?}");
            params.push(param_name.clone());

            if params.len() >= 255 {
                return Err(
                    "Cannot have more than 255 parameters in a function declaration.".to_string(),
                );
            }

            self.matching(TokenType::Comma);
        }
        self.consume(TokenType::RightParen, "Expect ')' for defining parameters.")?;

        self.consume(TokenType::LeftBrace, "Missing function block.")?;

        let body = self.parse_block_stmts()?;

        println!("[parse_func_decl_stmt] body: {body:?}");

        Ok(Statement::FuncDeclSt {
            name: func_name,
            params,
            body,
        })
    }

    // Parse zero or more statements
    fn parse_statements(&mut self) -> Result<Statements, String> {
        let mut statements: Vec<Statement> = Vec::new();
        // On success, we should be at the end
        while !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        Ok(Statements { statements })
    }
}

// # Top level parsing function.  This parses everything and makes sure there
// # are no unconsumed tokens left over.
pub fn parse(mut parser: Parser) -> Result<AST, String> {
    let statements = parser.parse_statements()?;
    Ok(AST { statements })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{Name, Op},
        reader::Source,
        tokenize::tokenize,
    };

    // Helper function to run a string through
    pub fn parse_expr(expr_text: &str) -> Expr {
        let source = Source::new("N/A", expr_text);
        let tokens = tokenize(source).expect("Failed to tokenize");
        // println!("\nTokens: {tokens:?}");
        let mut parser = Parser::new(&tokens);
        parser.parse_expression().expect("Failed to parse")
    }

    // Helper function to run a string through
    pub fn parse_statement(expr_text: &str) -> Statement {
        let source = Source::new("N/A", expr_text);
        let tokens = tokenize(source).expect("Failed to tokenize");
        // println!("\nTokens: {tokens:?}");
        let mut parser = Parser::new(&tokens);
        parser.parse_statement().expect("Failed to parse")
    }

    #[test]
    fn test_parse_addition() {
        let expr = parse_expr("2+5");
        dbg!(&expr);

        let expected = Expr::Binary {
            left: Box::new(Expr::TNumber(2.0)),
            op: Op::from_toktype(TokenType::Plus),
            right: Box::new(Expr::TNumber(5.0)),
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_assignment() {
        let expr = parse_expr("abc = 5;");
        dbg!(&expr);

        let expected = Expr::Assign {
            name: Name::new(TokenType::Identifier, "abc", 1),
            expr: Box::new(Expr::TNumber(5.0)),
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_logical_or() {
        let expr = parse_expr("true or false");
        dbg!(&expr);

        let expected = Expr::Logical {
            left: Box::new(Expr::TBoolean(true)),
            op: Op::from_toktype(TokenType::Or),
            right: Box::new(Expr::TBoolean(false)),
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_logical_and() {
        let expr = parse_expr("true and false");
        dbg!(&expr);

        let expected = Expr::Logical {
            left: Box::new(Expr::TBoolean(true)),
            op: Op::from_toktype(TokenType::And),
            right: Box::new(Expr::TBoolean(false)),
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_add_grouped_multiply() {
        let expr = parse_expr("2+(5*1)");
        dbg!(&expr);

        let expected = Expr::Binary {
            left: Box::new(Expr::TNumber(2.0)),
            op: Op::from_toktype(TokenType::Plus),
            right: Box::new(Expr::Grouping {
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::TNumber(5.0)),
                    op: Op::from_toktype(TokenType::Star),
                    right: Box::new(Expr::TNumber(1.0)),
                }),
            }),
        };

        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_print_statement() {
        let stmt = parse_statement("print \"hi there 123\";");
        dbg!(&stmt);

        let expected = Statement::PrintSt {
            expr: Expr::TString("hi there 123".to_string()),
        };

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_var_statement() {
        let stmt = parse_statement("var xyz;");
        dbg!(&stmt);

        let expected = Statement::VarSt {
            name: Token::new(TokenType::Identifier, "xyz", 1),
            expr: None,
        };

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_var_statement_initialized() {
        let stmt = parse_statement("var x = 1;");
        dbg!(&stmt);

        let expected = Statement::VarSt {
            name: Token::new(TokenType::Identifier, "x", 1),
            expr: Some(Expr::TNumber(1.0)),
        };

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_if_statement() {
        let stmt = parse_statement("if true { print 2; }");
        dbg!(&stmt);

        let expected = Statement::IfSt {
            condition: Expr::TBoolean(true),
            then_branch: Statements {
                statements: vec![Statement::PrintSt {
                    expr: Expr::TNumber(2.0),
                }],
            },
            else_branch: None,
        };

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_if_statement_with_cond_expression() {
        let stmt = parse_statement("if 3 > 1 { print 2; }");
        dbg!(&stmt);

        let expected = Statement::IfSt {
            condition: Expr::Binary {
                left: Box::new(Expr::TNumber(3.0)),
                op: Op::from_toktype(TokenType::Greater),
                right: Box::new(Expr::TNumber(1.0)),
            },
            then_branch: Statements {
                statements: vec![Statement::PrintSt {
                    expr: Expr::TNumber(2.0),
                }],
            },
            else_branch: None,
        };

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_if_else_statement() {
        let stmt = parse_statement("if false { print 2; } else {print \"hi there\";}");
        dbg!(&stmt);

        let expected = Statement::IfSt {
            condition: Expr::TBoolean(false),
            then_branch: Statements {
                statements: vec![Statement::PrintSt {
                    expr: Expr::TNumber(2.0),
                }],
            },
            else_branch: Some(Statements {
                statements: vec![Statement::PrintSt {
                    expr: Expr::TString("hi there".to_string()),
                }],
            }),
        };

        assert_eq!(stmt, expected);
    }

    #[test]
    fn test_parse_while_statement() {
        let stmt = parse_statement("while true { print 2; }");
        dbg!(&stmt);

        let expected = Statement::WhileSt {
            condition: Expr::TBoolean(true),
            body: Statements {
                statements: vec![Statement::PrintSt {
                    expr: Expr::TNumber(2.0),
                }],
            },
        };

        assert_eq!(stmt, expected);
    }
}
