# Simple Parser

## Basic Expressions
Arithmetic and function calls are as you would expect and similar to many other languages, such as JavaScript, C, etc.
```
10 + 2
```
```
10*x + 3/y
```
```
g(50*x) * testFunction(101)
```

## Statements
No semicolons.
### Assignment
Tradition assignment.
```
x = 10 + 3*y
```

TODO add more (and return statement)

## Functions
There are no function declarations. There are only lambda expressions, which are then assigned to a variable or object field. The syntax is the same as JavaScript lambda expressions (arrow functions).
```
myFunction = () => { x = 10 }
```
```
myOtherFunction = (a, b) => {
  x = a + b
  y = a * b
  z = x / y
}
```
