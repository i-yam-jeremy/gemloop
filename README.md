# GemLoop
A programming language that is mainly functional, but with some object-oriented concepts as well. I created this language because I like writing parsers, compilers and interpreters. If you find it useful or want to modify it please feel free to use it. You can try out and experiment with the following examples as you read in the [playground](https://i-yam-jeremy.github.io/gemloop).

## Basic Expressions
Arithmetic and function calls are as you would expect and similar to many other languages, such as JavaScript, C, etc. Strings behave as they do in JavaScript.
```
10 + 2.3
```
```
10*x + 3/y
```
```
g(50*x) * testFunction(101)
```
```
"Hello world!"
```
```
'hi' + " " + 'everybody'
```
```
"my number: " + 3
```

## Assignment
Variables can be assigned values with the traditional `var = value` syntax. This is not a statement, it is an expression that both sets the value of the variable in the scope and evaluates the the assigned value.
```
x = 10
```
```
x = y = 10
```
```
x = (y=3)+4
```

## Joint Expressions
There are no statements, however, there are joint expressions. The entire program is a single expression, but joint expressions can be used to combine multiple expressions into a single expression. The value of the joint expression is the value of the last expression within it.
```
x = 10,
y = 13,
x+y
```
## Comments
Single-line and multi-line comments are supported.
```
10 // this is a single-line comment
```
```
/*
 This is a multi-line comment
*/
10
```

## Functions
There are no function declarations. There are only lambda expressions, which are then assigned to a variable or object field. The syntax is the same as JavaScript lambda expressions (arrow functions). The return value of the function is simple the value of the expression that is the body. The body is a single expression, however joint expressions can be used to include multiple expressions.
```
myFunction = () => { x = 10 }
```
```
myOtherFunction = (a, b) => {
  x = a + b,
  y = a * b,
  z = x / y,
  x+y+z
}
```

## Classes
Class definitions are also expressions. Classes use an `init` method that is called on instantiation. For all methods, `this` is in scope and set to the object instance.
```
myClass = <> {
  init() => {
    this.x = 10
  }
  aMethod(x) {
    this.x + x
  }
}
```
```
myClass = <> {
  init(a) => {
    this.x = 10*a
  }
  aMethod(x) {
    this.x + x
  }
}
```
Parent classes can be set allowing class inheritance. Classes inherit all methods from parents (unless overridden) including the `init` method. Parent classes are set using expressions, so the parent class doesn't need to be known at compile-time, it can be the result of runtime operations.
```
myClass = <> {
  init(a) => {
    this.x = 10*a
  }
},
myChildClass = <>:myClass {
  anotherMethod(x, y) => {
    x+y
  }
}
```
```
myFunctionThatReturnsClass = (x, y) => {
  <> {
    init(a) => {
     this.x = 10*a + x + y // x and y are in a larger scope but it still works
    }
  }
},
myChildClass = <>:myFunctionThatReturnsClass(10, 2) {
  anotherMethod(x, y) => {
    x+y
  }
}
```
Class instantiation is as simple as calling a function. It is similar to Python in that a constructor is called in the same way functions are, with no `new` keyword.
```
myClass = <> {
  init(a) => {
    this.x = 10*a
  }
},
myClass(10)
```

## And you're done!
That's all there is to it. Feel free to make more in the [playground](https://i-yam-jeremy.github.io/gemloop)
