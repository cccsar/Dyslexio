# Gramática de LIPS-USB

## Gramática para la Documentación

### Entrada

    <entrada> -> <acción> | <exp>

### Acciones

    <acción> -> <def> | <assign>
    <def> -> lazy <tipo> | <tipo>;
    <tipo> -> int <assig> ; | bool <assign> ;
    <assign> -> <id> := <exp> ;

### Expresiones

    <exprs> -> <exp>
             | <exprs> , <exp>

    <exp> -> <exp1>
           | <id> ( <expes> )

    <exp1> -> <exp1> || <exp2>
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
    <exp5> -> <exp5> + <precdencia6>
            | <exp5> - <exp6>
            | <exp6>
    <exp6> -> <exp6> * <exp7>
            | <exp6> % <exp7>
            | <exp7>
    <exp7> -> !<exp7>
            | -<exp7>
            | +<exp7>
            | <exp8>
    <exp8> -> <exp9> ^ <exp8>
            | <exp9>
    <exp9> -> ( <exp> )
            | ' <exp> '
            | <id>
            | <entero>
            | true | false

## Gramática para la Implementación

```
%left '||'
%left '&&'
%nonassoc '=' '<>'
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '%'
%nonassoc '!'
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
  | '-' E
  | '+' E
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
