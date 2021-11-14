# Dyslexio

Interpreter for the LIPS programming language for the subject CI-3725 : Translators and Interpreters

## Lexer

### Testing instructions

This project compiles with ```ghc version 8.6.5```, and is intended for use in Unix systems only.

Run: 

> make && ./Main

And interact with Dyslexio REPL. 

Use

> make clean

To clean generated files.

---

The REPL consist of *special commands* and expressions to interpretate. Valid special commands are: 

* ```.lex EXPR ```: Returns the list of tokens from EXPR or a list of errors.
* ```.load FILENAME``` : Executes commands from file FILENAME as if they were directly written on the prompt.
* ```.failed``` : Prints the list of errors from a loaded file, or a message if no file has been loaded.
* ```.reset``` : Clear the list of erros from a loaded file, or outputs a message if no file has been loaded.
* ```.``` : Exits the prompt.

## Parser