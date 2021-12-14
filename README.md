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

The REPL consist of *special commands* and expressions to interpret. Valid special commands are: 

* ```.lex EXPR ```: Returns the list of tokens from EXPR or a list of errors.
* ```.load FILENAME``` : Executes commands from file FILENAME as if they were directly written on the prompt.
* ```.failed``` : Prints the list of errors from a loaded file, or a message if no file has been loaded.
* ```.reset``` : Clear the list of erros from a loaded file, or outputs a message if no file has been loaded.
* ```.ast EXPR|INSTR ```: Returns the AST for the instruction or expression in prefix notation or an error if parsing fails. For more information about the grammar of Dislexio refer to the 'Grammar' part of the this document.
* ```EXPR|INSTR ```: Invokes the interpreter and returns either the value of the expression's evaluation or an error if the interpretation fails. For more information about valid expressions and instructions in Dislexio refer to the 'About the Interpreter' part of this document.
* ```.``` : Exits the prompt.

---

## Grammar

The file ```gramatica.md``` contains a detailed description of the grammar used by Dislexio. The grammar is both in a human readable format and in a in the format used by ```happy```. The grammar is unambiguous and includes precedence and associative rules. 

A copy of the human readable format is presented next for clarity's:

Entrada

    <entrada> -> <instrucciones> | <exp>

Acciones

    <instrucciones> -> <instrucciones> <instruccion>
                     | <instruccion> 

    <instruccion> -> <tipo> <id> := <exp> ;
                   | <id> := <exp> ;

    <tipo> -> lazy <tipoBase>
            | <tipoBase>

    <tipoBase> -> int
                | bool

Expresiones

    <exprs> -> <exprs> , <exp>
             | <exp>

    <exp> -> <exp> || <exp2>
            | <exp2>
    <exp2> -> <exp2> && <exp3>
            | <exp3>
    <exp3> -> <exp4> = <exp4>
            | <exp4> <> <exp4>
            | <exp4>
    <exp4> -> <exp5> < <exp5>
            | <exp5> <= <exp5>
            | <exp5> > <exp5>
            | <exp5> >= <exp5>
            | <exp5>
    <exp5> -> <exp5> + <exp6>
            | <exp5> - <exp6>
            | <exp6>
    <exp6> -> <exp6> * <exp7>
            | <exp6> % <exp7>
            | <exp7>
    <exp7> -> ! <exp7>
            | - <exp7>
            | + <exp7>
            | <exp8>
    <exp8> -> <exp9> ^ <exp8>
            | <exp9>
    <exp9> -> ' <exp> '
            | <id> ( <exprs> ) 
            | ( <expr> ) 
            | <id>
            | <entero>
            | true 
            | false
            
##  About the Interpreter:
Dislexio's interpreter can evaluate any expression covered in the grammar showed before. Besides these 'trivial' expressions, Dislexio can also evaluate a series of special functions:
* ```if(<condition>, <expT>, <expF>) ```: Returns <expT> if the condition is true, otherwise returns <expF>. Both <expT> and <expF> must be of the same type and valid otherwise Dislexio will throw an error.
* ```type(<exp>)```: Returns the type of <exp>. The expression must be valid.
* ```ltype(<exp>)```: Returns the type of the content of the variable <exp>. The expression <exp> must be an LVALUE otherwise Dislexio will throw an error.
* ```cvalue(<exp>)```: Returns the content of the variable <exp>. The expression <exp> must be an LVALUE otherwise Dislexio will throw and error.
* ```reset() ```: Deletes every user defined variable from the VM. 
* ```irandom(n) ```: Returns a random integer in the range [0,n-1]. The argument n must be or evaluate to an integer bigger than 0, otherwise Dislexio will throw an error.
* ```fibo(n) ```: Returns the fibonacci number corresponding to n. The argument n must be or evaluate to a non negative integer, otherwise Dislexio will throw an error.
* ```gcd(<exp>, <exp1>) ```: Returns the greatest common divisor of <exp> and <exp1>. Both <exp> and <exp1> must be or evaluate to an integer, otherwise Dislexio will throw an error.
* ```now()```: Returns an integer corresponding to the time in milliseconds elapsed from a certain point in time.

 As of now only type, ltype, reset, irandom and fibo are implemented.
