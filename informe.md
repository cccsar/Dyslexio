
# Informe

## Extra functionalities of Dislexio

Considering the fact that integers are a very important part of Dislexio and that some mathematical functions like fibo and gcd are already implemented, we decided that it was important for Dislexio to have access to max and min functions as they are a pretty fundamental part of arithmetics.

### max: 
* Usage: ```max(<exp>, <exp1>) :: max := fn(int x, int y) => int```  
* Returns: ```the maximum between <exp> and <exp1>.```
* Explanation: The max function returns the maximum between two numbers passed as arguments. Both arguments must be integers otherwise Dislexio will throw an error.

### min: 
* Usage: ```min(<exp>, <exp1>) :: min := fn(int x, int y) => int```  
* Returns: ```the minimum between <exp> and <exp1>.```
* Explanation: The min function returns the minimum between two numbers passed as arguments. Both arguments must be integers otherwise Dislexio will throw an error.
