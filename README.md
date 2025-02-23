rulox
========================================

An interpreter for Lox language, from [Crafting Interpreters](https://craftinginterpreters.com/contents.html) written in Rust.
The interpreter covers till Chapter 10 (Functions) and cuts some corners such as we only accept two terms in an expression 
and only implemented while-loops.

This code was written when attending [David Beazley's Crusty Interpreter](https://www.dabeaz.com/crusty.html) immersion course.


## Running the code

```
cargo run -- loxcode/factorial.lox
```

## Resources

* Warmup exercise: 
    * [A tiny parser in Python](./warmup/tiny.py)
    * [A tiny parser in Rust](https://gist.github.com/jmsdnns/2bb7b43bfe0e44a076457a1123ca5e00)
* [Crafting Interpreters](https://craftinginterpreters.com)
* [Rust Programming Language](https://www.rust-lang.org)
* [How to Write a Lisp Interpreter](https://norvig.com/lispy.html). An overview of how to write a small Lisp interpreter (Peter Norvig).
