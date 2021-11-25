## Grammar representing understanding of LIPS 

### Input
`S -> Acc | Expr`

### Actions

```
Acc   -> Def | Assgn 
Def   -> lazy Type | Type;
Type  -> int Assgn ; | bool Assgn ;
Assgn -> <id> := Expr ;
```

### Expressions

```

Exprs -> Expr 
       | Exprs , Expr

Expr ->  E 
      | id ( Exprs ) 


E    -> E || P 
      | P  
P    -> P && Q
      | Q
Q    -> R = R 
      | R <> R 
      | R
R    -> S < S 
      | S <= S 
      | S > S 
      | S >= S 
      | S
S    -> S + T 
      | S - T 
      | T 
T    -> T * U 
      | T % U 
      | U
U    -> + U 
      | - U 
      | ! U 
      | V
V    -> W ^ V 
      | W
W    -> ( Expr ) 
      | ' Expr ' 
      | <id> 
      | <entero>
      | true | false
```

Associativities and precedences were explicitly stated.

Non terminals are identifiers, integer constants and boolean constants. They remain interleaved whithin expressions
cause the choice of whether an expression is correctly typed is left to the typesystem.


---


## Grammar specification extracted from parser generator definition

Associativies are described with __%left__ and __%right__ directives, meaning respectively left-to-right and right-to-left associativity.

They must be placed in order of increassing precedence.

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
