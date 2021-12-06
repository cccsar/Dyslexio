# Dyslexio

Interpreter for the LIPS programming language for the subject CI-3725 : Translators and Interpreters

## Required installations

This project compiles with ```ghc version 8.6.5``` , ```alex version 3.2.4``` and ```happy 1.19.12``` and is intended for Unix Systems only.

The dependencies listed before must be installed before using Dislexio.

These are the links with installations instructions for [Haskell Platform](https://www.haskell.org/downloads/), [alex](https://www.haskell.org/alex/) tool and [happy](https://www.haskell.org/happy/).

Alternatively after installing the [Haskell Platform](https://www.haskell.org/downloads/) you can install [alex](https://www.haskell.org/alex/) and [happy](https://www.haskell.org/happy/) using Cabal with these commands:
> cabal install alex

> cabal install happy

## Running instructions

You can compile and execute Dislexio REPL using this command:
> make && ./Main

Alternatively you can compile with:
> make

And later on execute the program with:
> ./Main

To clean generated files use:
> make clean

---
## Using the REPL

The REPL consist of *special commands* and expressions to interpretate. Valid special commands are: 

* ```.lex EXPR ```: Returns the list of tokens from EXPR or a list of errors.
* ```.load FILENAME``` : Executes commands from file FILENAME as if they were directly written on the prompt.
* ```.failed``` : Prints the list of errors from a loaded file, or a message if no file has been loaded.
* ```.reset``` : Clear the list of erros from a loaded file, or outputs a message if no file has been loaded.
* ```.ast EXPR|INSTR ```: Returns the AST for the instruction or expression in prefix notation.
* ```.``` : Exits the prompt.

---
## Grammar

The file ```gramatica.md``` contains a detailed description of the grammar used by Dislexio. The grammar is both in a human readable format and in a in the format used by ```happy```. The grammar is unambiguous and includes precedence and associative rules.
