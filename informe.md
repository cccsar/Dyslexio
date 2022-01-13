
# Informe

## Extra functionalities of Dislexio

Considering the fact that integers are a very important part of Dislexio and that some mathematical functions like fibo and gcd are already implemented, we decided that it was important for Dislexio to have access to functions for calculation maximum, minimum and least common multiple (between two numbers) as they are a pretty fundamental part of arithmetics.

### max: 
* Usage: ```max(<exp>, <exp1>) :: max := fn(int x, int y) => int```  
* Returns: ```the maximum between <exp> and <exp1>.```
* Explanation: The max function returns the maximum between two numbers passed as arguments. Both arguments must be integers otherwise Dislexio will throw an error.

### min: 
* Usage: ```min(<exp>, <exp1>) :: min := fn(int x, int y) => int```  
* Returns: ```the minimum between <exp> and <exp1>.```
* Explanation: The min function returns the minimum between two numbers passed as arguments. Both arguments must be integers otherwise Dislexio will throw an error.

### lcm: 
* Usage: ```lcm(<exp>, <exp1>) :: lcm := fn(int x, int y) => int```  
* Returns: ```the least common multiple between <exp> and <exp1>.```
* Explanation: The lcm function returns the least common multiple between two numbers passed as arguments. Both arguments must be integers otherwise Dislexio will throw an error.

## Fixes
* A minor bug where type('x') returned "int" instead of the expected "lazy int" was fixed.

## Known bugs
* There's supposedly an error with gcd but after testing the function we were unable to find anything wrong with it.
