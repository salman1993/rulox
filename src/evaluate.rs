use std::iter::zip;

use crate::ast::{Expr, Name, Op, Statement, Statements, AST};
use crate::environment::Environment;
use crate::tokenize::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    VNumber(f64),
    VString(String),
    VBoolean(bool),
    VNil,
    // Functions are objects with the same status of numbers, strings, etc.
    // This is created by the 'fun' declaration
    VFunction(Name, Vec<Name>, Statements),
}
use LoxValue::*;

pub fn interpret(ast: &AST) -> Result<LoxValue, String> {
    let mut environ = Environment::new();
    for stmt in ast.statements.into_iter() {
        execute(&stmt, &mut environ)?;
    }
    Ok(VNil)
}

fn execute(statement: &Statement, environ: &Environment) -> Result<LoxValue, String> {
    match statement {
        Statement::PrintSt { expr } => {
            let value = evaluate_expression(expr, environ)?;
            println!("{}", stringify(&value));
        }
        Statement::ExprSt { expr } => {
            let _ = evaluate_expression(expr, environ);
        }
        Statement::VarSt { name, expr } => {
            let value = if let Some(initializer) = expr {
                evaluate_expression(initializer, environ)?
            } else {
                VNil
            };
            environ.var(&name.lexeme, value);
        }
        Statement::BlockSt { body: statements } => {
            let block_env = Environment::new_child(environ);
            for stmt in statements.into_iter() {
                execute(stmt, &block_env)?;
            }
        }
        Statement::IfSt {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_val = evaluate_expression(condition, environ)?;
            match condition_val {
                VBoolean(cv) => {
                    if cv {
                        // execute then stmts
                        for stmt in then_branch.into_iter() {
                            execute(stmt, &environ)?;
                        }
                    } else if let Some(else_stmts) = else_branch {
                        // execute else stmts
                        for stmt in else_stmts.into_iter() {
                            execute(stmt, &environ)?;
                        }
                    }
                }
                _ => {
                    return Err( format!("The condition in 'if' statement must be a boolean, here is value: {condition_val:?}") );
                }
            }
        }
        Statement::WhileSt { condition, body } => {
            let condition_val = evaluate_expression(condition, environ)?;
            match condition_val {
                // enter the loop only if condition_val is true
                VBoolean(true) => {
                    loop {
                        for stmt in body.into_iter() {
                            execute(stmt, &environ)?;
                        }

                        // reevaluate condition
                        let cv_expr = evaluate_expression(condition, environ)?;
                        match cv_expr {
                            VBoolean(true) => continue,
                            VBoolean(false) => break,
                            _ => {
                                return Err( format!("The condition in 'while' statement must be a boolean, here is value: {condition_val:?}") );
                            }
                        }
                    }
                }
                _ => {
                    return Err( format!("The condition in 'while' statement must be a boolean, here is value: {condition_val:?}") );
                }
            }
        }
        Statement::FuncDeclSt { name, params, body } => {
            // A function definition needs to create a function "object".  This
            // object needs to be stored in the environment just like a normal
            // variable.
            //
            // Defining a function does *NOT* actually run the function. That's
            // done by a function call expression (see below).
            environ.var(
                &name.lexeme,
                VFunction(name.clone(), params.clone(), body.clone()),
            );
        }
        Statement::ReturnSt { expr } => {
            let retval = evaluate_expression(expr, environ)?;
            return Ok(retval);
        }
    }

    Ok(VNil)
}

fn stringify(x: &LoxValue) -> String {
    match x {
        VNil => "nil".to_string(),
        VNumber(v) => v.to_string(),
        VBoolean(v) => v.to_string(),
        VString(v) => v.clone(),
        VFunction(name, _, _) => format!("<Function {}>", name.lexeme),
    }
}

fn evaluate_expression(expr: &Expr, environ: &Environment) -> Result<LoxValue, String> {
    let value = match expr {
        // AST                  =>      Runtime
        Expr::TNumber(value) => VNumber(*value),
        Expr::TBoolean(value) => VBoolean(*value),
        Expr::TString(value) => VString(value.clone()),
        Expr::TNil => VNil,
        Expr::Grouping { expr } => evaluate_expression(expr, environ)?,
        Expr::Unary { op, expr } => evaluate_unary_op(op, expr, environ)?,
        Expr::Binary { left, op, right } => evaluate_binary_op(op, left, right, environ)?,
        Expr::Logical { left, op, right } => evaluate_logical_op(op, left, right, environ)?,
        Expr::Variable { name } => environ.lookup(&name.lexeme)?,
        Expr::Assign { name, expr } => {
            let value = evaluate_expression(expr, environ)?;
            // println!("[assign] name: {}, value: {:?}", name.lexeme, value);
            environ.assign(&name.lexeme, value.clone())?;
            return Ok(value);
        }
        Expr::Call {
            callee,
            arguments,
            paren,
        } => evaluate_call_expr(callee, arguments, paren, environ)?,
    };

    Ok(value)
}

// @Override
// public Object visitCallExpr(Expr.Call expr) {
//   Object callee = evaluate(expr.callee);

//   List<Object> arguments = new ArrayList<>();
//   for (Expr argument : expr.arguments) {
//     arguments.add(evaluate(argument));
//   }

//   LoxCallable function = (LoxCallable)callee;
//   return function.call(this, arguments);
// }

fn evaluate_call_expr(
    callee: &Expr,
    arguments: &Vec<Expr>,
    paren: &Token, // right parentheses token, useful for throwing error msgs
    environ: &Environment,
) -> Result<LoxValue, String> {
    let callee_value = evaluate_expression(callee, environ)?;

    // Early return if callee_value is not a function
    let VFunction(_name, params, body) = callee_value else {
        return Err(format!(
            "Error: {callee:?} is not a function - check line {}",
            paren.line
        ));
    };

    let arity = params.len();
    if arguments.len() != arity {
        return Err(format!(
            "Expected {arity} arguments but got {}.",
            arguments.len()
        ));
    }

    let mut evaluated_args = Vec::<LoxValue>::new();
    for arg in arguments {
        let arg_val = evaluate_expression(arg, environ)?;
        evaluated_args.push(arg_val);
    }

    // Create the function environment - "stack frame"
    // Then create the variable bindings of params -> argument values
    let global_env = environ.get_global_env();
    let function_env = Environment::new_child(global_env);
    for (param, argval) in zip(params, evaluated_args) {
        function_env.var(&param.lexeme, argval);
    }
    // println!("function_env: {function_env:?}");

    let mut retval: LoxValue = VNil;
    for stmt in body.into_iter() {
        retval = execute(stmt, &function_env)?;
    }

    return Ok(retval);
}

fn is_truthy(x: &LoxValue) -> bool {
    match x {
        VNil => false,
        VBoolean(v) => *v,
        _ => true,
    }
}

fn evaluate_unary_op(op: &Op, expr: &Expr, environ: &Environment) -> Result<LoxValue, String> {
    let value = evaluate_expression(expr, environ)?;
    let result_value = match (&op.toktype, &value) {
        (TokenType::Minus, VNumber(v)) => VNumber(-v),
        (TokenType::Bang, x) => VBoolean(!is_truthy(&x)),
        _ => {
            return Err(format!(
                "Unsupported unary operation: {:?} value: {:?}",
                op, value
            ));
        }
    };

    Ok(result_value)
}

fn is_equal(a: &LoxValue, b: &LoxValue) -> bool {
    match (a, b) {
        (VNil, VNil) => true,
        (VNil, _) => false,
        (x, y) => x == y,
    }
}

fn evaluate_binary_op(
    op: &Op,
    left_expr: &Expr,
    right_expr: &Expr,
    environ: &Environment,
) -> Result<LoxValue, String> {
    let left_val = evaluate_expression(left_expr, environ)?;
    let right_val = evaluate_expression(right_expr, environ)?;
    let result_value = match (&left_val, &op.toktype, &right_val) {
        // Numeric
        (VNumber(l), TokenType::Plus, VNumber(r)) => VNumber(l + r),
        (VNumber(l), TokenType::Minus, VNumber(r)) => VNumber(l - r),
        (VNumber(l), TokenType::Star, VNumber(r)) => VNumber(l * r),
        (VNumber(l), TokenType::Slash, VNumber(r)) => {
            if *r == 0.0 {
                return Err("Division by zero".to_string());
            }

            VNumber(l / r)
        }
        // String add
        (VString(l), TokenType::Plus, rlv) => VString(l.to_owned() + &stringify(rlv)),

        // Equality (== and !=)
        (x, TokenType::EqualEqual, y) => VBoolean(is_equal(&x, &y)),
        (x, TokenType::BangEqual, y) => VBoolean(!is_equal(&x, &y)),

        // TODO: see if we can make deduplicate some comparison code below
        // Numeric - <, <=, >, >=
        (VNumber(l), TokenType::Less, VNumber(r)) => VBoolean(l < r),
        (VNumber(l), TokenType::LessEqual, VNumber(r)) => VBoolean(l <= r),
        (VNumber(l), TokenType::Greater, VNumber(r)) => VBoolean(l > r),
        (VNumber(l), TokenType::GreaterEqual, VNumber(r)) => VBoolean(l >= r),
        // String - <, <=, >, >=
        (VString(l), TokenType::Less, VString(r)) => VBoolean(l < r),
        (VString(l), TokenType::LessEqual, VString(r)) => VBoolean(l <= r),
        (VString(l), TokenType::Greater, VString(r)) => VBoolean(l > r),
        (VString(l), TokenType::GreaterEqual, VString(r)) => VBoolean(l >= r),
        _ => {
            return Err(format!(
                "Unsupported binary operation: {:?} left: {:?}, right: {:?}",
                op, left_val, right_val
            ));
        }
    };

    Ok(result_value)
}

fn evaluate_logical_op(
    op: &Op,
    left_expr: &Expr,
    right_expr: &Expr,
    environ: &Environment,
) -> Result<LoxValue, String> {
    let left_val = evaluate_expression(left_expr, environ)?;
    let result_value = match (&left_val, &op.toktype) {
        // Short circuit
        (VBoolean(true), TokenType::Or) => VBoolean(true),
        (VBoolean(false), TokenType::And) => VBoolean(false),
        (VBoolean(_), _) => {
            let right_val = evaluate_expression(right_expr, environ)?;
            match right_val {
                VBoolean(_) => right_val,
                _ => {
                    return Err(format!("Error with logical op on line {}: right side must evaluate to a boolean value", op.line));
                }
            }
        }
        _ => {
            return Err(format!(
                "Error with logical op on line {}: left side must evaluate to a boolean value",
                op.line
            ));
        }
    };

    Ok(result_value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::{parse, Parser},
        reader::Source,
        tokenize::tokenize,
    };

    // Helper function to run a string, going through all of the interpreter steps
    pub fn run_expr(expr_text: &str) -> LoxValue {
        let source = Source::new("N/A", expr_text);
        let tokens = tokenize(source).expect("Failed to tokenize");
        println!("\nTokens: {tokens:?}");
        let mut parser = Parser::new(&tokens);
        let expr = parser.parse_expression().expect("Failed to parse");
        println!("\nExpr: {expr:?}");
        let environ = Environment::new();
        environ.var("x", LoxValue::VNumber(3.0));
        let value = evaluate_expression(&expr, &environ).expect("Failed to evaluate");
        value
    }

    // Helper function to execute a statement
    pub fn run_single_stmt(stmt_text: &str) -> () {
        let source = Source::new("N/A", stmt_text);
        let tokens = tokenize(source).expect("Failed to tokenize");
        // println!("\nTokens: {tokens:?}");
        let parser = Parser::new(&tokens);
        let ast = parse(parser).expect("Failed to parse");
        let stmt = ast.statements[0].clone();
        // println!("\Statement: {stmt:?}");
        let mut environ = Environment::new();
        environ.var("x", LoxValue::VNumber(3.0));
        execute(&stmt, &mut environ).expect("Failed to execute");
    }

    #[test]
    fn test_evaluate_literals() {
        assert_eq!(run_expr("2"), VNumber(2.0));
        assert_eq!(run_expr("2.1"), VNumber(2.1));
        assert_eq!(
            run_expr("\"hello 1234 world xyz10\""),
            VString("hello 1234 world xyz10".to_string())
        );
        assert_eq!(run_expr("true "), VBoolean(true));
        assert_eq!(run_expr("false"), VBoolean(false));
        assert_eq!(run_expr("nil"), VNil);
    }

    #[test]
    fn test_evaluate_comparison() {
        assert_eq!(run_expr("2 < 3"), VBoolean(true));
        assert_eq!(run_expr("2 <= 3"), VBoolean(true));
        assert_eq!(run_expr("2 > 3"), VBoolean(false));
        assert_eq!(run_expr("2 >= 3"), VBoolean(false));
        assert_eq!(run_expr("3 > 2"), VBoolean(true));
        assert_eq!(run_expr("\"hello\" == \"hello\""), VBoolean(true));
        assert_eq!(run_expr("\"z\" > \"a\""), VBoolean(true));
        assert_eq!(run_expr("\"hi\" == \"hello\""), VBoolean(false));
        assert_eq!(run_expr("\"z\" != \"a\""), VBoolean(true));
        assert_eq!(run_expr("\"z\" == \"a\""), VBoolean(false));
    }

    #[test]
    fn test_evaluate_group() {
        assert_eq!(run_expr("2 + (5 * 1)"), VNumber(7.0));
        assert_eq!(run_expr("(1/2)"), VNumber(0.5));
    }

    #[test]
    fn test_evaluate_binary() {
        assert_eq!(run_expr("2 + 3"), VNumber(5.0));
        assert_eq!(run_expr("2 * 3"), VNumber(6.0));
        assert_eq!(run_expr("2 - 3"), VNumber(-1.0));
        assert_eq!(run_expr("3 / 2"), VNumber(1.5));
    }

    #[test]
    fn test_evaluate_logical() {
        assert_eq!(run_expr("true and false"), VBoolean(false));
        assert_eq!(run_expr("true and true"), VBoolean(true));
        assert_eq!(run_expr("false or false"), VBoolean(false));
        assert_eq!(run_expr("false or true"), VBoolean(true));
        assert_eq!(run_expr("true and (1 == 2)"), VBoolean(false));
        assert_eq!(run_expr("false or (2 == 2)"), VBoolean(true));
    }

    #[test]
    fn test_evaluate_logical_short_circuit() {
        assert_eq!(run_expr("true or (1 / 0)"), VBoolean(true));
        assert_eq!(run_expr("(1 == 5) and (1 / 0)"), VBoolean(false));
    }

    #[test]
    fn test_evaluate_unary() {
        assert_eq!(run_expr("-5"), VNumber(-5.0));
        assert_eq!(run_expr("!(2 == 2)"), VBoolean(false));
        assert_eq!(run_expr("(-3) + 4"), VNumber(1.0));
        assert_eq!(run_expr("-3 + 4"), VNumber(1.0));
    }

    #[test]
    fn test_evaluate_lookup() {
        assert_eq!(run_expr("x+2"), VNumber(5.0));
        assert_eq!(run_expr("x-2"), VNumber(1.0));
        assert_eq!(run_expr("5-x"), VNumber(2.0));
        assert_eq!(run_expr("-x * 2"), VNumber(-6.0));
    }

    #[test]
    fn test_evaluate_assign() {
        assert_eq!(run_expr("x = 9;"), VNumber(9.0));
    }

    #[test]
    fn test_stringify() {
        assert_eq!(stringify(&VNumber(2.0)), "2");
        assert_eq!(stringify(&VNumber(2.00000)), "2");
        assert_eq!(stringify(&VNumber(2.0001)), "2.0001");
        assert_eq!(stringify(&VNumber(2.1)), "2.1");
        assert_eq!(stringify(&VNil), "nil");
        assert_eq!(stringify(&VBoolean(true)), "true");
    }

    #[test]
    fn test_execute_single_statements() {
        run_single_stmt("print \"hello 1234 world\";");
        run_single_stmt("print \"1 \n 2 \n   3\";");
        run_single_stmt("print x;");
        run_single_stmt("2 + (3*5);")
    }
}
