# tiny.py
#
# Tiny is a small calculator language.  It supports integers, three
# different math operations (+, -, *), grouping with parenthesis, and
# variables.   Here are some valid Tiny expressions:
#
#     2 + 3
#     2 + (3 * 4)
#     x = 2 + 3
#     10 + x
#
# Tiny does not allow more than two terms in a math operation. So
# "2 + 3 * 4" is illegal.  However, you can use parentheses to break
# it up into parts like "(2 + 3) * 4" or "2 + (3 * 4)".
#
# The code below implements Tiny.  The purpose is to illustrate the
# core features of a small programming language all in one place.
#
# In preparation for our Rust implementation of Crafting Interpreters,
# you might begin here.  See if you can translate the following
# code to Rust.  Doing this will require you to learn about Rust
# string handling, functions, and data structures.  

from dataclasses import dataclass

# -----------------------------------------------------------------------------
# Part 1 : Tokens
#
# As input, a programming language usually receives source code in the
# form of a string. A tokenizer takes the source string and breaks it
# into tokens.  Think of a token as a "word" in the programming language.
# Tiny has the following token types:
#
#    NUM     : One or more numerical digits. (example, 1234)
#    NAME    : One or more alphabetic characters (example, abc)
#    PLUS    : '+'
#    MINUS   : '-'
#    TIMES   : '*'
#    LPAREN  : '('
#    RPAREN  : ')'
#    ASSIGN  : '='

@dataclass
class Token:
    toktype : str
    tokvalue : str
    
def tokenize(text : str) -> list[Token]:
    tokens = [ ]
    n = 0
    while n < len(text):
        if text[n].isspace():
            n += 1
            continue
        elif text[n].isdigit():
            start = n
            while n < len(text) and text[n].isdigit():
                n += 1
            tokens.append(Token('NUM', text[start:n]))
        elif text[n].isalpha():
            start = n
            while n < len(text) and text[n].isalpha():
                n += 1
            tokens.append(Token('NAME', text[start:n]))
        elif text[n] in _literals:
            tokens.append(Token(_literals[text[n]], text[n]))
            n += 1
        else:
            raise SyntaxError(f"Bad character {text[n]!r}")
    return tokens

_literals = {
    '+' : 'PLUS',
    '-' : 'MINUS',
    '*' : 'TIMES',
    '(' : 'LPAREN',
    ')' : 'RPAREN',
    '=' : 'ASSIGN',
    }

# -----------------------------------------------------------------------------
# Part 2 : Parsing and Abstract Syntax
#
# Programs are represented as a data structure known as an abstract
# syntax tree (AST).  Construction of the AST is performed by a parser.
# The following code implements the AST and a recursive descent parser.

class Expr: pass

@dataclass
class Number(Expr):
    n: int
    
@dataclass
class Variable(Expr):
    name : str

@dataclass
class Assign(Expr):
    location : Expr
    value : Expr
    
@dataclass
class Add(Expr):
    left : Expr
    right : Expr

@dataclass
class Sub(Expr):
    left : Expr
    right : Expr

@dataclass
class Mul(Expr):
    left : Expr
    right : Expr

# Parsing is implemented by making a left-to-right scan through a list
# of tokens.  At a superfiscal level, scanning a list is a lot like
# iterating with a for-loop.  However, unlike iteration, parsing also
# involves the extra step of "peeking ahead."  The following class
# implements methods needed for this scanning process.
    
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.n = 0

    # Optionally accept the next token
    def accept(self, toktype) -> bool:
        if self.n < len(self.tokens) and self.tokens[self.n].toktype == toktype:
            self.n += 1
            return True
        else:
            return False

    # Return the last matched token
    def last(self):
        return self.tokens[self.n-1]

    # Check if we're at end of tokens
    def at_end(self):
        return self.n >= len(self.tokens)

# Top level parsing function.  This parses everything and makes sure there
# are no unconsumed tokens left over.
def parse(parser : Parser) -> Expr:
    expr = parse_expression(parser)
    # On success, we should be at the end
    if not parser.at_end():
        raise SyntaxError("syntax error")
    return expr

def parse_expression(parser : Parser) -> Expr:
    left = parse_term(parser)
    if parser.accept('PLUS'):
        return Add(left, parse_term(parser))
    elif parser.accept('MINUS'):
        return Sub(left, parse_term(parser))
    elif parser.accept('TIMES'):
        return Mul(left, parse_term(parser))
    elif parser.accept('ASSIGN'):
        return Assign(left, parse_expression(parser))
    return left

def parse_term(parser : Parser) -> Expr:
    if parser.accept('NUM'):
        return Number(parser.last().tokvalue)
    elif parser.accept('NAME'):
        return Variable(parser.last().tokvalue)
    elif parser.accept('LPAREN'):
        e = parse_expression(parser)
        if not parser.accept('RPAREN'):
            raise SyntaxError("Expected a )")
        return e
    else:
        raise SyntaxError("Expected a term")
        
# -----------------------------------------------------------------------------
# Part 3 - Interpreter
#
# An interpreter turns the AST into a value.  This involves walking over the
# AST structure, but also managing an environment for variables.

# The environment is a place to load/store variable values during evaluation.
class Environment:
    def __init__(self):
        self.vars = { }

    def assign(self, name: str, value: int):
        self.vars[name] = value

    def lookup(self, name:str) -> int:
        return self.vars[name]

# Evaluation works by recursively walking the AST and turning each
# AST node into a value.
def evaluate(e: Expr, environ: Environment) -> int:
    match e:
        case Number(value):
            return int(value)
        
        case Variable(name):
            return environ.lookup(name)

        case Assign(Variable(name), value):
            environ.assign(name, evaluate(value, environ))
            return environ.lookup(name)
            
        case Add(left, right):
            return evaluate(left, environ) + evaluate(right, environ)

        case Sub(left, right):
            return evaluate(left, environ) - evaluate(right, environ)
        
        case Mul(left, right):
            return evaluate(left, environ) * evaluate(right, environ)
        
        case _:
            raise RuntimeError(f"Can't evaluate {e}")

# -----------------------------------------------------------------------------
# Part 4 - REPL (Read-Eval Print Loop)
#
# The following code reads expressions from the user, evaluates it, and
# prints the final result.

def repl():
    environ = Environment()
    while True:
        line = input("calc > ")
        if not line:
            break
        try:
            tokens = tokenize(line)
            ast = parse(Parser(tokens))
            print(evaluate(ast, environ))
        except SyntaxError as err:
            print(err)

if __name__ == '__main__':
    repl()
    

    
    
