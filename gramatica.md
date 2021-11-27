# Gramática de LIPS-USB

## Gramática para la Documentación

### Entrada

`<entrada> -> <instrucciones> | <exp>`

### Acciones

```
    <instrucciones> -> <instrucciones> <instruccion>
                     | <instruccion> 

    <instruccion> -> <tipo> <id> := <exp> ;
                   | <id> := <exp> ;

    <tipo> -> lazy <tipoBase>
            | <tipoBase>

    <tipoBase> -> int
                | bool
```

### Expresiones

```
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
```

Se juntan tipos no "compatibles" en el reconocedor pues se considera que el verificar esto es parte del sistema de tipos.

## Gramática para la Implementación

Las precedencias en el generador de parser `happy` se precentan de menor a mayor, en orden de aparicion:

```
%left '||'
%left '&&'
%nonassoc '=' '<>'
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '%'
%nonassoc '!' NEG
%right '^'

%%

PROGRAM ::
    : INSTS
    | E

INSTS ::
        : INST
        | INSTS INST

INST ::
INST : TP id ':=' E ';'
     | id ':=' E ';'

TP ::
TP : lazy BASETP
   | BASETP

BASETP ::
BASETP : int
       | bool

ES ::
    : E
    | ES ',' E

E ::
E : numLiteral
  | true
  | false
  | '`' E '`'

  | E '+' E
  | E '-' E
  | '-' E %prec NEG
  | '+' E %prec NEG
  | E '*' E
  | E '%' E
  | E '^' E

  | E '<' E
  | E '<=' E
  | E '>' E
  | E '>=' E
  | E '=' E
  | E '<>' E

  | E '&&' E
  | E '||' E
  | '!' E

  | id '(' ES ')'
  | '(' E ')'
  | id
```
