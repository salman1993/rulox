use std::{
    ops::{Index, IndexMut},
    slice::{Iter, SliceIndex},
};

use crate::tokenize::Token;

// expression     → assignment ;

// unary          → ( "!" | "-" ) unary | call ;
// call           → primary ( "(" arguments? ")" )* ;
// arguments      → expression ( "," expression )* ;
// primary        → "true" | "false" | "nil"
//                | NUMBER | STRING
//                | "(" expression ")"
//                | IDENTIFIER ;
// assignment     → IDENTIFIER "=" assignment
//                | logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;

// expression   := literal | grouping | unary | binary ;
// literal      := NUMBER | STRING | "true" | "false" | "nil" ;
// grouping     := "(" expression ")" ;
// unary        := ("-" | "!") expression ;
// binary       := expression operator expression ;
// operator     :=  "==" | "!=" | "<" | "<=" | ">" | ">="
//                 | "+" | "-"  | "*" | "/" ;
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literal (T denotes terminal)
    TNumber(f64),
    TString(String),
    TBoolean(bool),
    TNil,
    Grouping {
        expr: Box<Expr>,
    },
    Unary {
        op: Op,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },
    Variable {
        // variable lookup
        name: Name,
    },
    Assign {
        name: Name,
        expr: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
        // It also stores the token for the closing parenthesis. We’ll use that token’s location when we report a runtime error caused by a function call.
        paren: Token,
    },
}

// Crafting interpreters uses the raw token to encode operators
// in BinOp and UnaryOp.
// The token encodes line number information so that could be useful
// if you wanted to issue an error message later.
pub type Op = Token;
pub type Name = Token;
pub type Parameters = Vec<Token>;

// program        → statement* EOF ;
// statement      → exprStmt
//                | forStmt
//                | ifStmt
//                | printStmt
//                | returnStmt
//                | whileStmt
//                | block ;
// returnStmt     → "return" expression? ";" ;
// exprStmt       → expression ";" ;
// printStmt      → "print" expression ";" ;
// block          → "{" declaration* "}" ;
// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExprSt {
        expr: Expr,
    },
    PrintSt {
        expr: Expr,
    },
    VarSt {
        name: Name,
        expr: Option<Expr>,
    },
    BlockSt {
        body: Statements,
    },
    IfSt {
        condition: Expr,
        then_branch: Statements,
        else_branch: Option<Statements>,
    },
    WhileSt {
        condition: Expr,
        body: Statements,
    },
    FuncDeclSt {
        name: Name,
        params: Parameters,
        body: Statements,
    },
    ReturnSt {
        expr: Expr,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statements {
    pub statements: Vec<Statement>,
}

impl Statements {
    pub fn new(statements: Vec<Statement>) -> Statements {
        Statements { statements }
    }

    pub fn len(&self) -> usize {
        self.statements.len()
    }

    pub fn get<I>(&self, index: I) -> Option<&I::Output>
    where
        I: SliceIndex<[Statement]>,
    {
        self.statements.get(index)
    }
}

// Index Trait: Implements the Index trait to allow immutable indexing.
impl Index<usize> for Statements {
    type Output = Statement;

    fn index(&self, index: usize) -> &Self::Output {
        &self.statements[index]
    }
}

// IndexMut Trait: Implements the IndexMut trait to allow mutable indexing.
impl IndexMut<usize> for Statements {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.statements[index]
    }
}

impl<'a> IntoIterator for &'a Statements {
    type Item = &'a Statement;
    type IntoIter = Iter<'a, Statement>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AST {
    pub statements: Statements,
}

impl AST {
    pub fn new(statements: Statements) -> AST {
        AST { statements }
    }
}

pub fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::TNumber(value) => format!("{value}"),
        Expr::TString(value) => format!("\"{value}\""),
        Expr::TBoolean(value) => format!("{value}"),
        Expr::TNil => "nil".to_string(),
        Expr::Grouping { expr } => format!("(group {})", format_expr(expr)),
        Expr::Unary { op, expr } => format!("({} {})", op.lexeme, format_expr(expr)),
        Expr::Binary { left, op, right } | Expr::Logical { left, op, right } => {
            format!(
                "({} {} {})",
                op.lexeme,
                format_expr(left),
                format_expr(right)
            )
        }
        Expr::Variable { name } => name.lexeme.clone(),
        Expr::Assign { name, expr } => format!("(assign {} {})", name.lexeme, format_expr(expr)),
        Expr::Call {
            callee: _,
            arguments: _,
            paren: _,
        } => format!("(call)"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenize::TokenType;

    #[test]
    fn test_format_expr_math() {
        // -2 * (5 + 1)
        let expr = Expr::Binary {
            left: Box::new(Expr::Unary {
                op: Op::from_toktype(TokenType::Minus),
                expr: Box::new(Expr::TNumber(2.0)),
            }),
            op: Op::from_toktype(TokenType::Star),
            right: Box::new(Expr::Grouping {
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::TNumber(5.0)),
                    op: Op::from_toktype(TokenType::Plus),
                    right: Box::new(Expr::TNumber(1.0)),
                }),
            }),
        };

        println!("Expression:\n{}", format_expr(&expr));
    }

    #[test]
    fn test_format_expr_var() {
        // var abc;
        let expr = Expr::Variable {
            name: Name::new(TokenType::Identifier, "abc", 1),
        };

        println!("Expression:\n{}", format_expr(&expr));
    }

    #[test]
    fn test_format_expr_assign() {
        // abc = 2.1;
        let expr = Expr::Assign {
            name: Name::new(TokenType::Identifier, "abc", 1),
            expr: Box::new(Expr::TNumber(2.1)),
        };

        println!("Expression:\n{}", format_expr(&expr));
    }
}
