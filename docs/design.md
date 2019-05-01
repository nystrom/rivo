# Design of the Ivo programming language

## Introduction

Ivo is a gradually-typed programming language that combines features of functional languages, logic languages, and object-oriented languages.

Ivo programs are gradually typed, providing the flexibility of dynamic typing while allowing a large class of errors cannot to be checked at compile time. Ivo’s type system is powerful, allowing you to easily model data. Ivo allows programmers to define their own data types, with control over how these are represented in memory. Ivo allows programmers to define their own operations using familiar syntax. Ivo allows programmers to easily write queries on their data.

#### Credits

Ivo was designed at the Università della Svizzera italiano in Lugano, Switzerland, and implemented by Nate Nystrom and Igor Moreno Santos. Ivo’s design was influenced by several programming languages. In particular, the type system is based on the Haskell type system, the syntax is based on Haskell, Ruby, and Python with a bit of Rust, Scala, and X10 thrown in. Traits were influenced by Haskell's type classes and Rust’s traits. The treatment of backward-mode functions and pattern matching were inspired by the language JMatch.

## Core language

We give the semantics of Ivo by translation into simpler constructs. This core language consists of just types and a few simple expressions. The formal semantics of the core are in the appendix. We present a grammar and informal semantics here.


## Program structure

An Ivo program is a set of modules. Modules provide a namespace for definitions.
A module consists of a set of declarations of values, functions, types, traits, etc.
Declarations may include expressions, formulas, patterns, and types.

## Lexical structure

#### Comments
Comments begin with `#`  and continue to the end of the line.

#### Unicode
Ivo supports Unicode-21 identifiers and operators.

#### Identifiers and reserved words
All strings of non-whitespace, printable characters are identifiers unless they are
reserved. Identifiers that begin with a letter may contain only letters and digits.
Identifiers that begin with a symbol must consist only of symbols.
Identifiers cannot begin with a digit.
Identifiers cannot contain whitespace.
Identifiers cannot contain the following symbols:

    . , @ # :
    ( )
    [ ]
    { }

Because Ivo has few built-in constructs, the language has very few reserved symbols relative to other languages. The reserved identifiers are:

    data do
    for fun 
    let
    module
    trait
    val var
    where
    ->
    _

#### Literals

##### Integers
Ivo provides several kinds of numbers.
Integer literals can be of any length.
A decimal integer matches the regular expression  `0 | [1-9] [_0-9]*`.
A binary integer matches the regular expression  `0b[_0-1]+`.
A hexadecimal integer matches the regular expression  `0x[_0-9a-fA-F]+`.
##### Rationals
Rational numbers can be written as `decimal . decimal e[+-]? decimal`, or as `hexadecimal . hexadecimal p[+-?] hexadecimal`.
Integer and rational literals are overloaded to support different number
representations.
##### Characters
A character literal is enclosed in single quotes. There are the usual escapes.
Ivo supports Unicode-21 characters.
Character literals are overloaded to support characters in different encodings.
##### Strings
A string literal is enclosed in double quotes. There are the usual escapes.
String literals are overloaded to support different string representations.
Strings can be implemented as lists, arrays, ropes, etc.

## Terms

There are two types of terms in Ivo: expressions and patterns.
All terms have types.

An expression is a term that evaluates to a value.

Patterns are terms with *unknown variables*. A pattern may be matched against an expression of the same type, binding the unknowns variables in the pattern to values. A pattern does not itself evaluate to a value. Patterns are used in function declarations and in binding formulas.

Pattern matching is performed on calls (to match arguments against parameters),
on binding and assignment (to match the right-hand-side expression against the left-hand-side pattern), and in other situations.
Patterns include unknown variables (written `let x` or sometimes just `x`),
the special wildcard pattern `_`, and invocations of functions that provide a backward mode.

A formula is a boolean pattern. A formula is matched against the boolean value 
`True`, binding any  unknown variables to values that satisfy the formula. Formulas are used in variable declarations and in `for` and `let` expressions.
Formulas may be used as declarations.

## Expressions

### Value literals

Any value literal is an expression that evaluates to the value of the literal.

`_` is the any literal.

`()` is the nil literal.

### Tuple literals

A tuple `(e1, e2, ..., en)` is equivalent to `Prelude::Tuple e1 e2 ... en`.

### List literals

The list literal `[e1, e2, e3]` is equivalent to `Prelude::Cons e1 (Prelude::Cons e2 (Prelude::Cons e3 Prelude::Nil))`.

### Set literals

The set literal `Set [e1, e2, e3]` is equivalent to `Prelude::Set::fromList [e1, e2, e3]`.

### Dictionary literals

The dictionary literal `Dict [e1 -> e2, e3 -> e4]` is equivalent to `Prelude::Dict::fromList [(e1, e2), (e3, e4)]`.

### Struct literals

The struct literal `{ x1: e1; ...; xn: en }` 

### Lambdas

The lambda `fun p1 p2 ... pn -> e` is an anonymous function that takes `n` arguments, binds them to the respective patterns, then evaluates `e` in the resulting environment.

### Bind expressions

The expression `p = e` matches the expression `e` against pattern `p`. If successful, `True` is returned; otherwise `False`.

### Generator expressions

The expression `p <- e` matches the stream `e` against pattern `p`. If any element of the stream matches, `True` is returned; otherwise `False`.

### Assignment expressions

The expression `p := e` matches the expression `e` against pattern `p`.
If successful, the `var` unknowns in `p` are overwritten.

### Select expressions

The expression `e.x` evaluates to the member `x` of the struct `e`.

If `e` is a stream, `e.x` is the stream of all members named `x` in any element of the stream.

### Union expressions

The expression `e1 with e2` evaluates to the stream containing the values of `e1` and `e2`.

### Variables

The expression `x` or `!x` evaluates to the value in variable `x`.

The pattern `x` or `?x` binds variable `x`

### Blocks

The expression `{ e1; e2; ...; en }` evaluates each of the expressions in turn.

A few language features introduce a block scope. Names defined inside the block are in scope
only within the block and within enclosing blocks, if not shadowed.
Variables, functions, data types, and traits defined within the block are in scope throughout the block.
These definitions can capture variables from the enclosing scopes.
Definitions in a block may be mutually recursive.
Forward references to non-lazy variables are illegal, however. It is not possible to use a variable in an expression
or a formula before the variable is defined.
If a variable is captured by a function (including within a trait or instance declaration), that function cannot be invoked before the variable is defined.
If a variable is captured by a data (constructor) declaration, that constructor cannot be invoked before the variable is defined.
The value of the block is the value of the last expression in the block.

## let expressions
A `let` expression tries to satisfy a formula, binding new variables as necessary. For example,

     let 2 + x == 5 {
         print x
     }
  
causes `x` to be bound to 3 in the body, which prints `x`. The value of a `let` expression is the value of the body. Variables bound in the `let` are in scope for the remainder of the enclosing block. `let`-bound variables are immutable.
The formula:

    let x :: xs == [1,2,3] {
        print x
        print xs
    }

binds `x` to 1 and `xs` to the list `[2,3]`.
It is a dynamic error for a formula used in a `let` to generate zero satisfying assignments or more than one satisfying assignment.
The `let` binds any variables in the formula not already bound. If all variables are bound already, the expression must evaluate to `True`.

## if expressions

An `if` expression is like a `let`, but does not generate an error if the formula does not have a solution.

    if 2 + x == 5 {
        print x
    }
An `if` expression can have an optional `else` clause, which is evaluated if there are no solutions to the formula.
The result of an `if` without an `else` is the unit value. The result of an `if` with an `else` is the result of whichever clause is evaluated.

## for expressions

A `for` expression is like a `let`, but evaluates the body for each solution, returning a stream of the results.

    # prints 1,2,3
    # evalutaes to the stream (),(),()
    for x in [1,2,3] {
        print x
    }

	# evaluates to the stream 2,3,4
	for x in [1,2,3] { x + 1 }

  
A `for` expression can have an optional `else` clause, which is evaluated if there are no solutions to the formula.
The result of a `for` expression is a stream of the `for` body results, or the empty stream if there is no `else` or the singleton stream of the `else` body.

## Var expressions
These are just like `let` expressions, but define mutable variables. For instance:

	var (2 + x == 5) {
	    x = x * 2
	    print x   // prints 6
	}

## Streams
A stream expression generates a possibly infinite sequence of values. There are several ways to define streams, including implementing the `Stream` trait. However, typically a stream can be generated from a formula. 
Given a formula, a stream can be created that iterates through all satisfying assignments of the formula. For instance, the formula `xs contains x && x > 0` iterates through all values in the collection `xs` that are greater than 0.
A stream may be empty, finite, or infinite.
Streams can also be created directly by implementing the `Stream` trait.
A `for` expression can iterate through a stream.
Any unknowns in the formula are bound in the body of the `for` .

The annotations are not needed since they can be inferred from the definition.

    fun while {@formula Boolean} do {@scope a} -> ()
    fun while {c} do {e} =
      if c then
        let _ = e
        while c do e
      else
        ()

nb. need tail recursion elimination to compile this efficiently.

    fun foreach {@formula Boolean} do {@scope a} -> ()
    fun foreach {f} do {e} = f.foreach { fun x: e[x] }
## Blocks

A few language features introduce a block scope. Names defined inside the block are in scope
only within the block and within enclosing blocks, if not shadowed.
Functions, data definitions, and traits, defined within the block are in scope throughout the block.
These definitions can capture variables.
Forward references to variables are illegal. It is not possible to use a variable in an expression
or a formula before the variable is defined.
If a variable is captured by a function (including within a trait or instance declaration), that function cannot be invoked before the variable is defined.
If a variable is captured by a data (constructor) declaration, that constructor cannot be invoked before the variable is defined.
NOT TRUE: for patterns, the value is a tuple. For formulas, there is an implicit `&&`.
The value of the block is the value of the last expression in the block.
If the last expression is a formula, the value is `True` or `False`.
If the last expression is a declaration, the value is `True`.

    do:
        fun f x = x + 1
        val x = f 2 + 3    # Bind
        (val y, val z) = (x, f x)
        [val w] = [1]
        val p = Point(1, 2)
        Point(val a, val b) = p
        print x
    
    
    fun f (Int) -> Int
    then later in the same file:
    fun (Int) + (Int) -> Int
    
    f 2 + 3
    should be
    (f 2) + 3
## Lists

The syntax `[1,2,3]` is syntactic sugar for `1:2:3:[]`.
The syntax `[1..10]` is syntactic sugar for `iterate from 1 to 10`.
The syntax `[1,3..10]` is syntactic sugar for `iterate from 1 then 3 to 10`.
The syntax `[1..]` is syntactic sugar for `iterate from 1` 
The syntax `[1,3..]` is syntactic sugar for `iterate from 1 then 3`.
The `iterate` functions are defined in the `Enum` trait, so implementers of the trait determine the semantics.
The default implementation:

    trait Enum a:
      fun iterate from (a) to (a) -> List[a]   where Enum a
      fun iterate from x to y = iterate from x to y using ( _ + 1 )
    
      fun iterate from x to y using f = if x > y then [] else x : iterate from (f x) to y
    
      fun iterate from x then y to z = if x > y then [] else x : iterate from y to z using (fun z: z + y - x)
    
      ...
## Names

A name is either an identifier or a parenthesized function name.
A function name is a sequence of identifiers and placeholders followed by `=` and another placeholder.
A placeholder is either `_`, indicating an input parameter, or `?`, indicating an output parameter.
For instance, `_ + _ = ?` is the name of the forward mode `+` operator, and

    _ + ? = _ is the name of one of the backward mode + operators.

In any function name, at least one of the placeholders must be `?`.
The forward mode binding placeholder (i.e., `= ?`) can be omitted. So `_ + _` is
another name for the forward mode `+` operator.

## Qualified names

A qualified name consists of a module name (which is just a qualified name)
a `.` and a simple name (as above).

## Let expressions

The expression `let` introduces variables into scope. `let` can be followed by a binding formula.

    let x = 3
    let [x,y,z] = [1,2,3]

The left-hand-side of the binding is a *pattern* and the right-hand-side is an *expression*.
Any variable name in the pattern is an *unknown*. The expression is evaluated and the pattern
is evaluated against the expression to bind the unknowns.
Patterns are first-class in Fretta. They're just functions which return the bound values.
Consider the following binding:

    let x + 2 = 5

This binds `x` to `3`, because `3` is the unique value for which the `x + 2` is `5`.
What's really happening is that the pattern `x + 2` is evaluated against `5`.
Indeed, the `let` is equivalent to the following binding:

    let x = (? + _ = _ ) 2 5

Here `? + _ = _` is a reference to a *backward mode* of the `+` operator that
takes one operand and the return value and returns the other operand.
Indeed `+` actually defines three modes:

    _ + _ = ?     # forward mode
    ? + _ = _     # backward mode: solve for the first operand
    _ + ? = _     # backward mode: solve for the second operand

A `let` can also be followed by an arbitrary boolean formula. The formula is evaluated in
a backward mode to find the solution to any unknowns. It is a compile-time error if there is more than one solution.
For instance, the following is equivalent to the last binding above:

    let x + 2 == 5   # binds x to 3, since 3 is the unique solution to x + 2 == 5

This is equivalent to:

    let x = (? + _ = _ ) 2 ((? == _ = _ ) 5 True)

In principle, `+` could have another mode:

    ? + ? = _

In this mode, there is not a unique solution and calling the function will return
a stream of all possible solutions.
The backward modes of `+` are defined as:

    fun (-> x) + y = z
      where
        x = z - y
    
    fun x + (-> y) = z
      where
        y = z - x

The backward modes of `==` are:

    fun (-> x) == y = True
      where
        x = y
    
    fun x == (-> y) = True
      where
        y = x
    
    fun (-> x) == y = False
      where
        x = [x where x: a, x != y]    # iterate through all values of type a, except x
        x = undefined                 # or just fail

A `where` expression lets us get a stream of all the satisfying assignments of a formula.

    [x where 0 < x && x <= 10]

Streams are lazy and can be infinite.

    fretta> let xs = [x: Int where x > 0]
    xs :: Stream[Int] = _
    fretta> head xs
    1

Only the first element of `xs` is computed when the `head` is taken. The other elements are not computed until forced.
Since `xs` is an infinite stream, the following will go into an infinite loop testing if each value is less than 10. Haskell has the same behavior with list comprehensions. We could in principle decide for some streams whether they are infinite or not, but this is in general undecidable.

    fretta> let zs = [x where ys contains x && x odd && x < 10]
    zs :: Stream[Int] = _
    fretta> list zs
    [1,3,5,7,9   (loop)
## Streams

An invertible function generates a stream if it has an iterator mode.
A type ascription `p: T` generates a stream if `T` implements trait `Bounded` and `Inc`.
These traits can be implemented by default for some data types (cf. `deriving`).


## Exceptions

`throw e`  evaluates expression `e` and throws it as an exception.
`e1 catch e2` evaluates `e1`. If an exception is thrown by `e1`, the exception value is
passed to the function `e2`. If `e2`'s parameter pattern does not match the exception value, the exception is rethrown.
The `undefined` expression throws the `Error("undefined")` exception value.
`error e` evaluates the string `e` to string value `v` and throws a `Error(v)` exception value.
`e1 finally e2` evaluates `e1`. Regardless of how `e1` terminates, `e2` is evaluated. The result of the computation is the value of `e1` . `e2` is evaluated for any side effects.

## Data definitions

A data definition introduces a fixed-size type. The body of a data definition is either a pattern specifying the
constructor argument(s) for values of the type, or a sequence of constructor definitions.
A constructor definition consists of a constructor name and a sequence of patterns.
For example, the following is data definition for a two-dimensional point struct.

    data Point:
      let x: Float
      let y: Float

A data definition defines a constructor to create values of the data type. The constructor has the same name as the type and takes an argument that must match the patterns in the definition.
In the case above, the definition induces a constructor function names `Point`:

    fun Point (Float) (Float) -> Point
    fun Point (let x: Float) (let y: Float) = (inject :: (Float,Float) -> Point) (x, y)

[N.B. The `inject` function is a compiler intrinsic of type `a -> b` that can be used only in constructor implementation. It cannot be called directly by client code. It's really only there to make the type-checker happy. Operationally it adds (or replaces) a tag on an injected value.]
To create a `Point`, we invoke the `Point` constructor passing in two `Float` values.

    Point 1.2 3.4

The representation of a value of the data type is a tuple (struct) consisting of all the unknowns in the constructor pattern.
Fields of the struct can be accessed using the field selector functions.
The data definition induces the accessor definitions:

    fun (Point) . x -> Float
    fun (Point x y) . x = x
    
    fun (Point) . y -> Float
    fun (Point x y) . y = y

A data definition may optionally provide different constructors, which will result in different representations of the data type.
For example:

    data Point:
      case Point1:
        let x: Float
      case Point2:
        let x: Float
        let y: Float
      case Point3:
        let x: Float
        let y: Float
        let z: Float

It is required that fields of the same name have the same type. This ensures the selector functions are well-typed and can be
implemented as one function with many alternatives.

    fun (Point) . x -> Float
    fun (Point1 x) . x = x
    fun (Point2 x y) . x = x
    fun (Point3 x y z) . x = x

Fields defined in different data definitions may have the same name.

    data Circle:
      let radius: Float
    
    data Cylinder:
      let radius: Float
      let height: Float

This results in overloading of the selector functions:

    fun (Circle) . radius -> Float
    fun (Cylinder) . radius -> Float

Constructors (like other functions) can be overloaded. Constructor invocation is identical to function invocation.
Constructor patterns may be empty. For instance the `Boolean` type is defined as:

    data Boolean:
      case False
      case True

Values of a data type are represented as a tagged union of the structs defined by the constructors.
This means a `Boolean` can be represented by its tag. Data types with only one case have no tag.
Thus, the unit type `()` has no runtime representation at all since it has no cases and no fields.

    data ()

Data definitions cannot be recursive. They may not contain fields of the same type as being defined, nor types defined by data definitions that include the defined type. Nor can their type parameters be instantiated on the same type?
Data definitions may include fields of class type which contain fields of the data type. This is because class instances are implemented using references.
**TODO is the distinction between classes and data useful at all?**

## Class definitions

A class definition introduces a reference type. The body of a class definition is similar to the body of a data definition, however they can be recursive.

    class IntList:
      case Nil
      case Cons:
        let head: Int
        let tail: IntList

The compiler checks that the recursion is well-founded (that is, at least one of the choices must be non-recursive). This prevents classes that cannot be
instantiated. [TODO maybe this should be a warning? They can be instantiated if `let` behaves likes `letrec`.]

    # an infinite list of 0
    let xs = Cons 0 xs
## Type parameters

Data and class definitions may be declared with type parameters. For example:

    data Option[A]:
      case None
      case Some:
        let value: A
    
    class List[A]:
      case Nil:
      case Cons:
        let head: A
        let tail: List[A]
## Inheritance

Data and class definitions can extend others. There is no subtyping however.
TODO: Not sure this is useful here.

## Types

Each data definition defines a type constructor.
Each class definition defines a type constructor.
Function types are defined by the built-in class constructor `->`.
Tuple types are defined by the built-in data constructors '(,)', `(,,)`, etc.
The unit type is defined by the built-in data type `()`.

## Function definitions

Functions are declared with the `fun` keyword. A function declaration starts with a signature, which specifies the name, parameter types, and return type of the function, and is followed by the implementation. The signature is optional if it can be inferred.
Function declarations can be used to declare mix-fix operators. Symbols used in the function declaration are part of the function name.
**Signatures**
A function signature consists of a sequence of identifiers and parenthesized types, then an `->` and a return type.
The signature must include at least one identifier and at least one parameter type.
The following function signature is for a function named `inc`, which takes an integer and returns an integer.

    fun inc (Int) -> Int

The following function signature is for an infix `+` operator on integers.

    fun (Int) + (Int) -> Int

If curly brackets are used instead of parentheses, the argument is call-by-name rather than call-by-value.
For example, the following operator defines short-circuiting boolean AND:

    fun (Boolean) && {Boolean} -> Boolean

When invoked, the second argument is not evaluated unless used in the function body.
Functions can be used to define mix-fix operators. For example, the ternary function `if` is defined as:

    fun if (Boolean) then {a} else {a} -> a

Parameters may be double-parenthesized (or double-bracketed) to indicate operator associativity. Thus, with the following signature, `+` is left associative:

    fun ((Int)) + (Int) -> Int

With the following signature `::` is right associative:

    fun (a) :: ((List[a])) -> List[a]

At most one argument type can be double-parenthesized.
**Alternatives**
After the signature, zero or more alternatives can be provided. The alternatives are evaluated in order. The body of the first matching alternative is invoked.
Each alternative is given by the function name and patterns for each argument, and then the function body.

    fun inc (Int) -> Int
    fun inc (let x) = x + 1
    
    fun if (Boolean) then {a} else {{a}} -> a
    fun if (True) then (let e) else _ = e
    fun if (False) then _ else (let e) = e
    
    fun (Int) + (Int) -> Int
    fun 0 + (x) = x
    fun (x) + 0 = x
    fun (x) + (y) = iadd x y
## Expressions

**Primary expressions**
Literals
Variable names
Selectors
Simple function calls
Parenthesized and bracketed expressions, lists, tuples, etc.

## Mix-fix functions

Mix-fix operators may be declared with `fun` definitions. A function is *mix-fix* if its signature
includes a symbol identifier or any identifier after the first position.
Each module generates a set of parser rules for the mix-fix functions it defines.
If `E` is the parser nonterminal for expressions
and if `P` is the nonterminal for primary expressions, then
the parser is extended as follows for a module with function definitions `f1`, ... `fn`.

    E ::= E1
    E1 ::= r1
         | E2
    E2 ::= r2
         | E3
    ...
    En ::= rn
         | P

where `ri` is generated as follows from definition `fi`:

    fun x1 T1 x2 T2 ... xk Tk -> T
    
    Ei ::= x1 E{i+1} x2 E{i+1} ... xk E{i+1}
         | E{i+1}

If there is an associativity annotation on a type, `Ei` is generated on the RHS
instead of `E{i+1}`.
If there is more than one functions with the same name, a rule is generated
for the first such function. The actual function call is resolved using the overloading rules below.
When a module is imported, the parser is extended with the rules generated from the functions
in the imported module.
Since multiple functions may have the same name or overlapping names, parsing using these rules may be ambiguous.
In this case, the compiler will report an error for any expression that cannot be parsed
unambiguously. The programmer can usually resolve the error by adding parentheses.
TODO: need to be more precise, need to allow overloading.
**Invertible Functions**
Functions in Fretta can be declared to support several *modes* of evaluation.
Normal evaluation is referred to as *forward mode*. A function can be invoked in a *backward mode* by passing in its return value and optionally some arguments, yielding the other arguments.
A function invoked in a backwards mode can be used as a *pattern* or to define a *stream*.
A function that supports both forwards and backwards modes is called *invertible*.
An invertible function can be used as both a function and as a pattern for
matching.
Constructors are always invertible functions.
An expression is invertible if it calls invertible functions.
Backwards modes are automatically generated if the function body is invertible.
Backward modes can be define explicitly by inverting the return type arrow in the signature (`<-`) and annotating
the returned arguments with arrows `->`. The "body" of the function is given by a pattern and a where
clause is used to define the returned arguments. For example, here is an `inc` function declared in forward mode:

    fun inc (Int) -> Int
    fun inc (let x) = x + 1

ALTERNATIVE:

    fun inc _ :: Int -> Int
    fun inc (Int) = (? Int)
    fun inc (let x) = x + 1

Here is the backwards mode:

    fun inc (? Int) <- Int
    fun inc (? x) = (let y)
      where:
        x = y - 1

The "function body" is just a pattern that matches the return value.
The `where` clause is used to specify the code to evaluate to bind the argument.
Variables in the argument and return value patterns are in scope in the `where` clause.
As shorthand, the returned argument expression can be written inline.

    fun inc (? Int) <- Int
    fun inc (? y - 1) = (let y)

The compiler can automatically generate backward modes. See [Romanenko PEPM'91].
We can explicitly write the different signatures and the compiler will check if the different modes can be generated.

    fun inc (Int) -> Int
    fun inc (? Int) <- Int
    fun inc (x) = x + 1

A backward mode can also generate a stream of results. This is done by defining several alternatives which bind the unknown argument variable differently.

    fun (List[a]) contains (? a) <- Boolean
    fun [] contains (? z) = False
    fun (x::xs) contains (? z) = True
      where:
        z == x || xs contains z

Higher-order functions can also be invertible. Here, is the forward mode for `map`, which takes
a function as an argument.

    fun map (a -> b) (List[a]) -> List[b]
    fun map (f) [] = []
    fun map (f) (x::xs) = f x :: map f xs

In the backward mode, the type of the function argument is inverted.

    fun map (b -> a) (? List[a]) <- List[b]
    fun map (f) (? []) = []
    fun map (f) (? f x :: map f xs) = (x::xs)

Using this invertible `map`, we can apply the inverted function to each element of a
list.

    let map inc (let xs) == [1,2,3]
    # binds xs to [0,1,2]

Thanks to Luis Mastrangelo for the example.
Another example: `zip` and `unzip`:

    fun zip (List[a]) (List[b]) -> List[(a,b)]
    # forward
    fun zip xs [] = []
    fun zip [] ys = []
    fun zip (x::xs) (y::ys) = (x,y)::zip xs ys
    # backward
    fun zip (-> map (fun xy: xy.1) xys) (-> map (fun xy: xy.2) xys) <- xys

Then we can define `unzip` as:

    fun unzip xys = (xs,ys)
      where:
        zip xs ys == xys
    where introduces a formula context. The body of a backward function

is often a formula.

    zipWith:
    # forward mode
    fun zipWith ((a,b) -> c) (List[a]) (List[b]) -> List[c]
    zipWith (f) (xs) [] = []
    zipWith (f) [] (ys) = []
    zipWith (f) (x::xs) (y::ys) = f x y :: zipWith f xs ys
    
    # backward mode
    fun zipWith (c -> (a,b)) (-> List[a]) (-> List[b]) <- List[c]
    fun zipWith (f) (-> xs) (-> ys) = zs
      where:
        xys = map f zs
        xs = map (fun xy: xy.1) xys
        ys = map (fun xy: xy.2) xys
    zipWith3, etc. can be defined similarly.

Operations and their inverses:

    fun (a) ** (Nat) -> a where Num a
    fun (x) ** 0 = 1
    fun (x) ** 1 = x
    fun (x) ** 2 = x * x
    fun (x) ** 3 = x * x * x
    fun (x) ** 4 = x * x * x * x
    fun (x) ** n where even n =
      let y = x ** (n/2)
          y * y
    fun (x) ** n =
      let y = x ** (n/2)
          y * y * x
    
    fun sqrt (a) -> a where Fractional a
    fun sqrt (x :: Double) = java.lang.Math.sqrt(x)
    fun sqrt (x :: Float)  = java.lang.Math.sqrt(x.toDouble).toFloat
    
    # backward mode
    fun sqrt (-> y * y) <- y

**Invertible function invocation**
Functions can be invoked in forward mode in any expression. For example, consider the function with the signature:

    fun (List[a]) contains (a) -> Boolean
    fun (List[a]) contains (-> a) <- Boolean

We can invoke the function in an `if` expression, in forward mode:

    if ([1,2,3] contains 1) then "yes" else "no"
    # evaluates to yes

Or we can invoke the function in backward mode as a stream:

    [[1,2,3] contains (let x)]

This yields a stream of the values 1, 2, and 3, bound to `x`.
In forward mode, if any argument pattern does not match, then the function call fails with an exception.
If used in an `|` expression, if an argument pattern in the function called in the left operand fails,
the second function is called instead.
Functions can be invoked in backward mode in patterns and in streams.
If used in a pattern, if the return value pattern or any argument pattern does not match, then a pattern match using the function fails.
If used in a stream, if the return value pattern or any argument pattern does not match, then the result stream is empty.

## Function overloading

Multiple functions may have the same name. For a function call, the compiler selects the unique function in scope that has a correct type
and highest priority.
TODO: this is incorrect: two functions can have the same name and different types.
Priority is for determining the name of the function during parsing. This should be in another section.
After priority is determined, overloading resolution is performed for *all* functions in scope with the same name (modulo hiding).
The priority is given in the following order:

- A function defined in an inner scope has priority over a function in an outer scope.
- If two functions are defined in the same scope, the later function has priority.
- If two functions are imported from the same module, the function defined later in the module has priority.
- If two functions are imported from different modules, neither has priority and there is a static error.

Constructors can be overloaded also.
For both data and class definitions, one can declare overloaded constructors by defining a function with same name as the constructor.
Open issues
Okay, this is a problem.... Overloading + parametric polymorphism is undecidable (Duggan ICFP'95: "Duggan and Ophel demonstrate that, in any type system for multi-parameter parametric overloading which is rich enough to support the example in Fig. 4, type-checking is undecidable") or NP complete. There are various proposals to help here. First parametric overloading (Stefan Kaes, LISP 92 and an earlier paper), which is a precursor of Haskell type classes. Type classes, themselves (Wadler and Blott '92). Mixin modules in ML (Duggan and Sourelis, ICFP'95).
Example: import two modules: Int + Int and Float + Float.
Both are in scope and have the same (that is incompatible) priority.
So x + y unconstrained should be an error.
We could unify into a type class (which has what priority over the imports?)
We could unify into an anonymous type class.
Solution? Generalize the types using a union type constraint:
_ + _ resolves to a -> a -> a where a \in {Int,Float}
Collect constraints like we do with type classes.
If ambiguous at the end, *specialize* the functions.
So:
import Int
import Float

    add x y = x + y

would have type:

    add : a -> a -> a where a in {Int, Float}

Then generate specializations:

    add : Int -> Int -> Int
    add x y = (_ + _ : Int -> Int -> Int) x y
    
    add : Float -> Float -> Float
    add x y = (_ + _ : Float -> Float -> Float) x y

In general, we have an exponential blowup in code. But this might be tolerable.
Also we won't have the overhead of dictionaries in many cases.
Constraint union:

    Int -> Int -> Int   ++   Float -> Float -> Float
    ==>
    a -> a -> a where a in {Int, Float}

Maybe just infer types like this and push unions

    a where {a: Int -> Int -> Int} | {a: Float -> Float -> Float}

unification of `a` with another type needs to prune the constraints.
So new implementation of type checking:

    add constraints to preds.
    if unification causes a constraint to be false, remove it
    but one of the constraints in an | must hold, right?
    so need to check consistency at the end.
    can't just end up with
    
    add : a -> a -> a
    
    a -> a -> a where {a: Int} | {a: Float} | {a: false}

Parsing:

- each function definition introduces a parser rule
- but then we have an ambiguous grammar when we import overloaded functions!!!
    - these get merged into one rule
    - we can then parse (possibly with some more ambiguities resolved by priority rules)
- ambiguities are resolved during type inference
- prefix functions always have higher priority that infix or mixfix??? or what???

Can we define juxtaposition as an operator like in Maude? This is useful for implementing function application syntax for collections.

    fun (Array[a]) (Nat) -> a
    fun a i = if (i < a.length) then (a.data get i) else error "bounds"

Also the empty unary operator for coercions:

    fun (Int) -> Long   # implicit conversion from Int to Long

The type-checker needs to ensure the conversion is not done unless needed, but the overloading rules should guarantee that.

## Constraints

One can also specify constraints on types. Fretta supports the following types of constraints:

- *Method constraints*: a type implements a particular function
- *Equality constraints*: two types are equal
- *Functional dependency constraints*: one type uniquely determines another

Fretta also supports conjunctions of the above constraints. Constraints can be named.
Constraints can be conditional, that is, implied by other constraints.
Constraints can be added to function signatures with a `where` clause.
**Method constraints**
A method constraint is given by a function signature. For example, the following
constraint says that type `a` has an `inc` function:

    fun inc (a) -> a

We can use the constraint in another function as follows:

    fun add2 (a) -> a where fun inc (a) -> a
    fun add2 (x) = inc (inc x)

In this example, `add2` can call `inc` even though the argument type is just
a type variable `a`. `add2` can be called with a value of any type as long as
that type has an `inc` function.
Another example:

    fun sum (List[a]) -> a where fun (a) + (a) -> a

A method constraint must have at least one type variable.
Implementation note
In Haskell, when doing type inference, we infer class instance constraints.
This works well because the only function in scope with a given name is defined in
a single type class. Consider:

    class Inc a where:
      inc :: a -> a
    
    add2 x = inc (inc x)

The compiler infers the type for add2:

    add2 :: Inc a => a -> a

The analogue with method constraints doesn't work so well. Consider the overloaded functions:

    inc (Int) -> Int
    inc (Char) -> Boolean
    inc (Boolean) -> Char
    
    add2 x = inc (inc x)

We infer one of two completely different types.

    add2 _ : Int -> Int
    add2 _ : Char -> Char

This ambiguity should be an error. Instead, we can infer:

    add2 _ : a -> b where fun inc (a) -> c
                          fun inc (c) -> b

Again this should be an error because we have two occurrences of `inc` in the
signature.
During type inference, if we have two method constraints with the same name and arity,
we merge them into one signature by unifying the type variables.
To avoid unification, we can have an explicit signature on the function.
**TODO: need to work out how type inference works**
**Type equality constraints**
An equality constraint is given as `type T1 = T2`. Equality constraints are useful for defining type aliases.

    type IntList = List[Int]

They are also useful with higher kinds. [See `T1 ~ T2` constraints in Haskell.]
**Traits: named conjunctions of constraints**
A trait is a conjunction of type constraints over the same type variables.

    trait Inc a:
      fun inc (a) -> a
      fun dec (a) -> a

The trait is satisfied by any type that has `inc` and `dec` functions.
If we define the functions that implement the trait.

    fun inc (Int) -> Int
    fun inc x = x + 1
    
    fun dec (Int) -> Int
    fun dec x = x - 1

Then, implicitly the predicate `Inc Int` is satisfied.
If we explicitly assert the predicate `Inc Int`, the compiler will check that the available functions exist.
In general functions are written to expect the trait to be implemented on a type variable (e.g., `Inc a`), not on a concrete type.

    Inc Int

Here is a trait for numbers:

    trait Num a:
      fun - (a) -> a
    
      fun (a) * (a) -> a
      fun (a) / (a) -> a
      fun (a) % (a) -> a
    
      fun (a) + (a) -> a
      fun (a) - (a) -> a

The trait declaration introduces the name of the trait (`Num` above) and one or more type parameters (`a` above).
The trait includes declarations for functions on the type parameters. Any types implementing the trait, must provide the given functions.
We can define default implementations of traits as top-level functions:

    fun - (a) -> a where Num a
    fun - (x) = 0 - x

A type can override the default implementation by providing a definition later
in the same module (i.e., following the usual priority rules).
Classes and data definitions can implement a trait simply by providing the necessary definitions:

    fun - (Bit) -> Bit
    fun - x = x
    
    fun (Bit) * (Bit) -> Bit
    fun 0 * _ = 0
    fun 1 * x = x
    
    ...

The type parameters can be parameterized types. Here is a `List` trait, for example.

    trait List l[a]:
      fun [] -> l[a]
      fun (a) :: (l[a]) -> l[a]
      fun (l[a]) ++ (l[a]) -> l[a]
      fun head (l[a]) -> a
      fun tail (l[a]) -> l[a]

This is syntactic sugar for a trait on two parameters with a functional dependency constraint
on the parameters.

    trait List l a:
      l -> a
    
      fun [] -> l
      fun (a) :: l -> l
      fun (l) ++ (l) -> l
      fun head (l) -> a
      fun tail (l) -> l

The annotation `l -> a` is a *functional dependency* and specifies that `a` is uniquely determined by `l`.
Like other functions, functions specified in method constraints (and therefore in traits)
can be run in both forward and backward modes. Thus, traits can be used to overloaded patterns.
For example, the `::` function on the `List` trait is invertible and can be used in a pattern to
match any value that implements the `l` variable of the `List` trait.
As a syntactic convenience, a trait declaration can require other constraints:

    trait Num a where Ord a:
      ...

is equivalent to:

    trait Num a:
      Ord a
      ...

This means any type implementing `Num` must also implement `Ord`. Here's a more complicated example:

    trait NumList l a where List l a, Num a:
      fun sum (l) -> a
      fun product (l) -> a
      fun min (l) -> a
      fun max (l) -> a

TODO: Traits can also by using to implement *named instances* (Kahl et al., 2001) ?
**Functional dependency constraints**
The constraint `T -> U` indicates that `T` uniquely determines `U`. These constraints are useful in defining traits.

    trait List l a:
      l -> a
    
      fun head (l) -> a
      fun tail (l) -> l
      fun (a) :: (l) -> l
      fun (l) ++ (l) -> l
## Type system

Fretta implements a version of the Damas-Hindley-Milner type system.
Indeed, the type system is an instance of HM(X), where X is includes equality,
type class (trait), and overloading constraints. The compiler generates a constraint
for each binding group, which is solved to compute the types.
The base type system is extended with constraints (similarly to type classes in Haskell).
Overloading is handled by generating constraints on the types of variables and then
rejecting infeasible constraints. Functions that access overloaded variables are specialized on the overloading.
**Partial application (currying)**
A function can be applied partially by filling in the writing `_` for some arguments.
For example,

    1 + _

is a function that takes a single argument and adds 1 to it.
As with overloaded calls, ambiguities can be resolved using a type ascription.

    1 + _ : Int -> Int

This works only in forward mode. Backwards modes are similar, but all backwards arguments must be provided, the return value can be partially applied by writing `= _`.
Another way to say this is that the arguments can be put in place of the placeholders in the function name. If not all argument placeholders (i.e., `_`) are provided, the expression evaluates to a function that takes the remaining arguments.

## Literals and syntactic sugar

As in Haskell, literals are overloaded, but we go further and support overloading of not just numbers, but also strings and lists and tuples.
An integer literal `2` is sugar for `fromInteger 2`, where `2` is an `BigInteger`. Obviously, the compiler will optimize this.
A floating point literal `3.14` is sugar for `fromRational 3.14`.
A String literal `"abc"` is sugar from `fromString "abc"`.
The list syntax `[e1, ..., en]` is sugar for `e1 : ... : en : []`, where `:` and `[]` are overloadable.

    trait Seq s a:
      fun [] -> s a
      fun (a) : (s) -> s a

The list generator syntax `[0..10]` and `[0..]` and the list comprehension syntax are also overloadable.
A Tuple `(e1, ..., en)` is sugar for `tuple{n} e1 ... en`, where `tuple{n}` is overloadable.

    trait Tuple2 t a b:
       fun tuple2 (a) (b) -> t a b
       fun (t a b) .1 -> a
       fun (t a b) .2 -> b
## Data types

The following data types are defined in the library:

    Int (arbitrary precision)
    Int8, Int16, Int32,Int64,Int128`
    Rat (arbitrary precision)
    Float32, Float64 
    Bit 
    Word8, Word16, Word32, Word64, Word128 
    Boolean 
    Char 
    Real constructive reals
    Complex a complex numbers
    Point2 a 2-d points (vectors)
    Point3 a 3-d points (vectors)

Type classes:

    Num a all numbers
    Real a real numbers
    FloatingPoint a all floats
    Integral a all integers
    Percentage a where Real a between 0 and 100
    Probability a where Real a between 0 and 1
    Point a n-d points (implemented by Point2 etc)
## Transactional memory

Fretta supports transactional memory. Here is an atomic counter:

    class Counter:
      var v: Int
    
    fun getAndInc (Counter) -> Int
    fun getAndInc (c) =
      atomic:
        c.v = c.v + 1
        c.v

Normal variables can be used in transactions.
For Java backend, Can build on top of `DeuceSTM`, which just lets you annotate methods as `@Atomic`.
Just compile all `var` variables to `STMVar` and use Scala's implementation.
For gcc backend, can use `gcc-4.7 -fgnu-tm`.

## Modules

Import a module with `import`.
Export definitions with `export`.
Can re-export imported definitions. This is how Prelude works... imports

    Boolean, Int, etc and re-exports. This ensures precedence works correctly.

Precedence in imported modules is determined by the order of `export` in the module.
Modules must explicitly export definitions to be used in other modules.
Import declarations:

    import Module
    open Module
    open Module hiding Name
    
    import Module.Name
    import Module.Name as AnotherName

Export declarations:

    export fun (a) + (a) -> (a)
    export trait T
    export data D
    export val x

**Open issues**
Modules can be parameterized like ML functors?
Modules are not first-class. Consider making them so. `module` defines a singleton `data`; `object` defines a singleton `class`.
Should `import` behave like Scala? Like Haskell?

## Prelude

The standard library includes data and function definitions for `Boolean`, `Int`, etc., which are imported explicitly.
These are defined in the library `fretta.lang`.
The built-in word types are in the module `fretta.native`, not in the Prelude.

## Unknowns

An unknown is either a let expression, `let x`, or a wildcard expression, `_`.
It is a compile-time error to evaluate a wildcard expression in forward mode.

## Arrays

Arrays are just classes declared as follows:

    class Array[a]: (val length: Nat, val data: a{length})

Note the `data` field of an array is dependent on the `length`.

## Syntax

To support mix-fix with indentation based syntax, need to support : or \n as special operators.

    while c do:
        s
    : introduces a block expression.

We adopt the Scheme convention of ending boolean functions with `?` and non-pure functions with `!`. This is just a convention.
Definitions introduced in a block have the scope of the block.

## Function expressions

All functions are partial functions. If the parameter pattern doesn't match, an exception is thrown.
A function can be tested to check if it is defined for a particular argument with `defined?`.

    fun (a -> b) (a) defined? -> Boolean

IMPLEMENTATION NOTE:

    defined? is generated for each function value. We lookup defined? in the closure structure (which has the environment and the code pointer for the function itself, and the code pointer for defined?). defined? is generated as a function fun p: True | _ : False which has the same pattern match p as the function.

Partial functions can be composed with `|` ("or else").

    fun (a -> b) | (a -> b) -> (a -> b)
    fun (f | g) = fun x: if f x defined?
                    then f x
                    else g x

We can define:

    fun match (a) with (a -> b) -> b
    fun match (x) with (f) = f x

Thus the Haskell expression:

    case e of P1 -> e1 | P2 -> e2 | ...

can be written:

    match e with (fun P1: e1 | fun P2: e2 | ...)

Maybe make `case` a synonym for `fun` 

    match e with (case P1: e1 | case P2: e2 | ...)
## Varargs

We can use overloading to implement varargs.

    trait Varargs v a:
      fun (v) ... -> List[a]
    
    # instantiate on tuples -- just do as Scala and bake this in up to length 22
    # functions are all invertible
    fun () ... -> List[a]
    fun () ... = []
    fun (a) ... -> List[a]
    fun (x) ... = [x]
    fun (a,a) ... -> List[a]
    fun (x,y) ... = [x,y]
    fun (a,a,a) ... -> List[a]
    fun (x,y,z) ... = [x,y,z]
    
    fun reduce (a -> a -> a) (v) where Varargs v a
    fun reduce (f) (xs ...) = xs reduce f
    
    data FormatArg:
      case IntArg: let value: Int
      case FloatArg: let value: Float
      case StringArg: let value: String
      ...
    
    fun printf (String) (v) where Varargs v FormatArg

**Java interaction**
Would be good for portability and access to libraries to run on JVM.
Would be nice to interact with Apache Spark, for example.
A Java class or interface is implemented as a trait:

    trait Object a:
      fun (a) . hashCode -> Int
      fun (a) . equals (b) -> Boolean  where Object b
      fun (a) . toString -> s          where String s
    
      # constructors
      fun new Object -> a

And an opaque data type needed for `new` to work:

    class Object
    Object Object
    
    let x = new Object

Here's a subclass:

    trait String a:
      Object a
    
      fun (a) . charAt (Int) -> Char
      fun (a) . substring (Int) (Int) -> b  where String b
    
      # constructors
      fun new String -> a
      fun new String (b) -> a  where StringBuffer b
    
    class String
    Object String
    String String
    
    let sb = new StringBuffer
    sb.add("123")   # need implicit conversions
    let s = new String(sb)   # or sb.toString

How do we import Java classes

    import java.lang.Object  # compiler knows how to generate necessary glue

How do we pass Fretta values to Java?
Java objects combine a type with a set of traits into a single value.
Need to package up a value and its trait instances into a single object?
Need to implement Java traits?

    let x = new Object   # create a Java object
    let y = new Object
    ...
    x equals y           # pass Java object to Java ... easy
    
    let z = D
    
    # implement the Java Object trait
    Object D where:
    
    x equals (Object z)  # pass Fretta value to Java

The Java interface provides also constructors for creating Java objects from Fretta values.
If you implement a Java trait, the wrapper is generated

    Object D where:
      fun (D) hashCode -> Int
      fun (this) hashCode = default
      fun (D) equals (a) -> Boolean where Object a
      fun (this) equals (that) = default

This is where we need `deriving`...

    # compiler automatically generates the instance wrappers.
    default Object D
    
    # or we have for java.lang.Object at least default implementations
    import java # import the default wrappers
    
    Object a where Eq a:
      fun (a) hashCode -> Int
      fun (a) equals (b) -> Boolean where Object a:
      fun x equals y = x == y

Issues: if we're wrapping objects, how do we preserve identity?
Wrappers need to handle this. Can we do this with 0 overhead?
What do other languages do? Clojure? JRuby, etc?
In JRuby, can we pass JRuby values into Java methods? Yes, but as
wrapped object, right?
JRuby:

    require 'java'
    require 'path/to/mycode.jar'
    java_import java.io.File
    
    f = File.new("hello")

Generics:

    trait Collection l a:
        l -> a
        fun (l) add (a) -> Boolean
        fun (l) addAll (c) -> Boolean      where Collection c (? extends a)
        fun (l) clear -> ()
        fun (l) contains (o) -> Boolean    where Object o
        fun (l) containsAll (c) -> Boolean where Collection c (?)
        fun (l) isEmpty -> Boolean
        fun (l) iterator -> i              where Iterator i a
        fun (l) remove (o) -> Boolean      where Object o
        fun (l) removeAll (c) -> Boolean   where Collection c (?)
        fun (l) retainAll (c) -> Boolean   where Collection c (?)
        fun (l) size -> Int
        fun (l) toArray -> Array[a]
        fun (l) toArray (Array[b]) -> Array[b]  where Object b

But wildcards?

    ? extends T
    ? super T
    
    Need to add subtyping constraints to Fretta?
    
    # s is the superclass of c
    trait Extends c s:
        c -> s

Every Fretta trait becomes an interface.
Subclassing of Java classes in Fretta: can use in Java code?
Can bake in subclassing Object.
Need to handle *not* subclassing final classes like String.
For others, hmm.

    class C {
      void m();
    }

becomes:

    trait C a:
      Object a
      fun (a) m -> ()

Then let's subclass C:

    data D
    
    # implement Object using autogenerated wrapper
    default Object D
    
    # implement C's methods
    fun (D) m -> ()
    fun (x) m = ()
    
    C D  # assert that D is an instance of trait C

Then we have in Java the following method:

    static void n(C x) { x.m(); }

In Fretta:

    fun n (a) -> () where C a

We can call it:

    val x = D
    n x

But how does it appear in Java?

    class D$adapter extends C implements fretta.Adapter {
      M$.D$impl self;       // reference to the Fretta D value in module M
      void m() {
        M$.m(self);         // call the Fretta m function (in module M) with a D
      }
      int hashCode() { return self.hashCode(); }
      boolean equals(Object other) {
        if (other instanceof fretta.Adapter) {
          fretta.Adapter a = (fretta.Adapter) other;
          return self.equals(a.self());
        }
        return false;
      }
    }

Subclassing of Fretta classes by Java code (or at least implementing Fretta traits)?
Not so important.

    trait I a:
      fun (a) add (Int) -> a
    
    class C implements fretta.I {
      I add(int x) { return this; }
    }

Mapping of Fretta modules to Java packages?
Can pass any Fretta function in for a Java interface. Compiler will wrap up
the function value in a proxy of the right type.

## Cake pattern with traits?

Can traits also provide types?
Sure, why not? Is there any advantage to this? We can do the cake pattern, maybe?

    trait Trees t u where Types u:
      class t:
        case Bin:
          left: Tree
          right: Tree
          typo: u
        case Value:
          value: Int
          typo: u
    
    trait Types t:
      class t:
        case IntType

You can instantiate the trait by implementing all the required cases. This is just normal traits with constructors treated the same as normal functions. But also a constraint that `t` be a class.
**TODO: in the implementation do we need to distinguish between data and class kinds?**

    data Compiler:
      data Type:
        case IntType
        case BoolType
        case FunType: Type Type
    
      data Tree:
        case Bin: ...
        case Value: ...
        case Lambda: ...
    
      Trees Tree Type
      Types Type

**Lazy variables and memoized functions**
Should we have `lazy` variables? Or do we just need streams?
Similarly, introduce `memo` functions. Do these need to be a language feature or can they be implemented in a library:
fun memo (a -> b) -> (a -> b)
fun memo f:
let val cache = Map[a,b]
fun x:
let val y = cache(x)
if empty? y then
let val y = f x
add! (x, y) to cache
y
else
y
THOUGHT:
when computing transitive closure (for instance), can we avoid keeping track of the visited set... just memoize the results

    derives a b = cyclic (false) (a == b || exists c in a . derives c b)

**Constraint checking**
Type-checking is performed using a constraint store rather than a simple typing environment.
The constraint store contains facts. A fact can be added to the store using an assertion or contract or using a conditional branch. The fact is asserted with the condition is true and its negation is asserted when the condition is false.
Constraints are used in partial evaluation to optimize code. Constraints are used to report contract failures statically.
Programmers can add *equations* to extend the constraint solver. Equations are basically (bidirectional) rewriting rules for constraint terms.
Constraints and equations are interpreted in the logic of uninterpreted functions. The system is unsound if the equations do not hold in the implementation. However, the type system remains sound at least.
**Equations**
Programmers can also specify *equations*. The compiler can use these equations
to check and optimize code. Semantically, the compiler can replace any
expression under evaluation that matches one side of an equation
with the other side.
Examples:

- Algebraic constraints, rings, fields, groups, etc.
# + is commutative
- eqn x + y == y + x
- Set constraints
# De Morgan's Laws
- eqn ~(x | y) == ~x & ~y
- eqn ~(x & y) == ~x | ~y
- Fold and map laws
# map
- eqn map f (map g xs) == map (f . g) xs
- eqn map f (xs ++ ys) == map f xs ++ map f ys
# append
- eqn xs ++ [] == xs
- eqn [] ++ ys == ys
- eqn xs ++ (ys ++ zs) == (xs ++ ys) ++ zs
- Monad laws
- eqn (return x) >>= f == f x
- eqn m >>= (f >>= g) == (m >> f) >>= g
- Equivalence relations
- eqn x == x == True
- eqn x == y == y == x
- eqn (x == y) && (y == z) == x == z
- Specialization
- fun toDouble (Real) -> Double
- fun toDouble x == fromRational (toRational x)
- eqn toDouble (Int) == i2d x
- eqn foldr (*+*) 0 == sum
- eqn foldl (*+*) 0 == sum
- eqn foldl f z [] == z
- eqn foldl f z [x] == f z x

Equations can be grouped into traits with the operations.
The compiler assumes the equations are true for any type implementing the constraints.
Here are some traits:

    trait Semigroup a:
      fun (a) + (a) -> a
    
      # the group operation is associative
      eqn (x + y) + z == x + (y + z)
    
    trait Monoid a:
       Semigroup a
    
       # adds identity
       fun 0 -> a
       eqn x + 0 == x
       eqn 0 + x == x
    
    trait Group a:
       Monoid a
    
       # adds inverse
       fun - (a) -> a
    
       fun (a) - (a) -> a
       eqn x - y == x + (-y)
    
       eqn x - x == 0
    
    trait AbelianGroup a:
       Group a
    
       # adds commutativity
       eqn x + y == y + x
    
    trait Rng a
       AbelianGroup a
    
       # an abelian group for + and a semigroup for *
       fun (a) * (a) -> a
    
       Semigroup a where:
          fun x + y == x Rng.* y
    
       eqn x * (y + z) == (x * y) + (x * z)
       eqn (x + y) * z == (x * z) + (y * z)
    
    trait Ring a
       Rng a
       # an abelian group for + and a monoid for *
    
       Monoid a where:
          fun x + y == x Ring.* y
          fun 0 == Ring.1
    
    trait Lattice a:
       fun (a) /\ (a) -> a  # meet
       fun (a) \/ (a) -> a  # join
       fun (a) <= (a) -> Boolean
    
       # commutative
       eqn a \/ b == b \/ a
       eqn a /\ b == b /\ a
    
       # associative
       eqn a \/ (b \/ c) == (a \/ b) \/ c
       eqn a /\ (b /\ c) == (a /\ b) /\ c
    
       # absorption -- only laws where meet and join interact
       # otherwise meet and join form two different semilattices
       # these laws make meet and join duals
       eqn a \/ (a /\ b) == a
       eqn a /\ (a \/ b) == a
    
       # idempotent
       eqn a \/ a == a
       eqn a /\ a == a
    
       eqn a == a /\ b
    
    trait BoundedLattice a
       Lattice a
    
       val bot: a
       val top: a
    
       # identity
       eqn a \/ bot == a
       eqn a /\ top == a
    
    trait DistributiveLattice1 a
       Lattice a
       eqn a \/ (b /\ c) == (a \/ b) /\ (a \/ c)
    
    trait DistributiveLattice2 a
       Lattice a
       eqn a /\ (b \/ c) == (a /\ b) \/ (a /\ c)
    
    trait ModularLattice a:
       DistributiveLattice1 a
       DistributiveLattice2 a
       eqn (a /\ c) \/ (b /\ c) == ((a /\ c) \/ b) /\ c
    
    trait Set s a:
       fun empty -> s
       fun singleton (a) -> s
    
       fun (s) subset (s) -> Boolean
       fun (s) contains (a) -> Boolean
    
       eqn (singleton x) contains y == x == y
       eqn s contains x == (singleton x) subset s
    
       eqn empty subset _ == True
       eqn empty contains _ == False
    
       fun (a) in (s) == s contains a
    
       Rng s where:
           fun x + y == x | y
           fun x * y == x & y
           val 0 == empty
    
       Lattice s where:
           meet == |
           join == &
           <= == subset
    
       fun (s) | (s) -> s
       fun (s) & (s) -> s
    
       eqn x | empty == x
       eqn x | x == x
       eqn x | y == y | x
       eqn x | (y | z) == (x | y) | z
    
       eqn x & empty == empty
       eqn x & x == x
       eqn x & y == y & x
       eqn x & (y & z) == (x & y) & z
    
       eqn x & (y | z) == (x & y) | (x & z)
    
       fun ~ (s) -> s
       eqn ~ (x | y) == ~x & ~y
       eqn ~ (x & y) == ~x | ~y
## Cake pattern

Cake pattern is good for cooperative development, but not separate development.
See Rossberg and Dreyer on MixML.

    trait Tree { type Exp }
    trait Eval { self : Tree with Normalize }
    trait Normalize { self : Tree with Eval }
    object Interpreter extends Tree with Eval with Normalize

Problem is all the implicit dependencies.... Can we name them explicitly?
can then name components:

    trait Eval { t: Tree; n: Normalize }
    trait Normalize { t: Tree; e : Eval }
    object Interpreter extends Tree with Eval { t = this; n = this }
                                    with Normalize { t = this; e = this }
## Supercompilation

Consider all possible rewrites of an expression.
Use machine learning to select the best rewrite.
Classify as advantageous or not.
Include things like introducing accumulators.

## Case expressions

Borrowed from Fortress.

    case planet element of
      {"Mercury", "Venus", "Earth", "Mars"} => "inner"
      {"Jupiter", "Saturn", "Uranus", "Neptune"} => "outer"
      _ => "none"

Compare all cases with >

    case most > of
       1 mile => yes
       1 km => no
    end

<!--stackedit_data:
eyJoaXN0b3J5IjpbMTMxMDI1NDU0MywxMTEyMzEyOTUxLC04ND
I1MTA5MCwtMTM2OTU4MzI3OSwtOTk0Njk0MjcwXX0=
-->