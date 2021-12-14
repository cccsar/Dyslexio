# Dyslexio

Interpreter for the LIPS programming language for the subject CI-3725 : Translators and Interpreters

## Required installations

This project compiles with ```ghc version 8.6.5``` , ```mtl 2.2.2```, ```System.Random```,  ```alex version 3.2.4```and ```happy 1.19.12``` and is intended for Unix and Windows systems only.

The dependencies listed before must be installed before using Dislexio.

### Installation using Cabal (recommended):

Download and install the [Haskell Platform](https://www.haskell.org/) following the instrutions on https://www.haskell.org/downloads/. 

The [Haskell Platform](https://www.haskell.org/) includes [Cabal](https://www.haskell.org/cabal/). Before using Cabal you should always refresh the package index using:
> cabal update

After installing the [Haskell Platform](https://www.haskell.org/) and updating Cabal; install [alex](https://www.haskell.org/alex/) and [happy](https://www.haskell.org/happy/) using Cabal with these commands :

> cabal install alex
> 
> cabal install happy

Lastly you must install [mtl](https://hackage.haskell.org/package/mtl) and [System.Random](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html) as libraries(IMPORTANT!). To to this with Cabal use these commands:

> cabal install --lib random
> 
> cabal install --lib mtl

Note that the previous instructions were tested on Windows.

For UNIX systems you may need to replace ```cabal install package``` for ```cabal v2-install package``` and ```cabal install --lib package``` for ```cabal v2-install --lib package --package-env .``` while on Dislexio's folder.

### Other methods:

Download and install the [Haskell Platform](https://www.haskell.org/) following the instrutions on https://www.haskell.org/downloads/.

If you prefer installing the dependecies manually here are the links for every one of them [alex](https://www.haskell.org/alex/), [happy](https://www.haskell.org/happy/), [mtl](https://hackage.haskell.org/package/mtl) and [System.Random](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html).

If you prefer to use other haskell tools like Stack for installing dependencies, the process should be similar to the Cabal installation; the most important thing to remember is that [mtl](https://hackage.haskell.org/package/mtl) and [System.Random](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html) must be exposed so that Dislexio can use them.

---
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
* ```now()```: Returns an integer corresponding to the time in milliseconds elapsed from the Unix epoch (00:00:00 UTC on 1 January 1970).
