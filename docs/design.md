# Design of the Ivo programming language

## Introduction

Ivo is a gradually-typed programming language that combines features of functional languages, logic languages, and object-oriented languages.

Ivo programs are gradually typed, providing the flexibility of dynamic typing while allowing a large class of errors cannot to be checked at compile time. Ivo’s type system is powerful, allowing you to easily model data. Ivo allows programmers to define their own data types, with control over how these are represented in memory. Ivo allows programmers to define their own operations using familiar syntax. Ivo allows programmers to easily write queries on their data.

### Credits

Ivo was designed at the Università della Svizzera italiano in Lugano, Switzerland, and implemented by Nate Nystrom and Igor Moreno Santos. Ivo’s design was influenced by several programming languages. In particular, the type system is based on the Haskell type system. Traits are influenced by Haskell's type classes and Rust's traits. Open type and function definitions are based on the research language J& plus polymorphic variants in OCaml and some research extensions of Haskell. The syntax is based on Haskell with a bit of Rust, Scala, and X10 thrown in. The treatment of backward-mode functions and pattern matching were inspired by the research language JMatch plus the LogicT library from Haskell. Macros were inspired by Rust and Racket. 

### Organization

An Ivo program is a set of modules. Modules provide a namespace for definitions. A module consists of a set of declarations of variables, functions, data types, and traits. Declarations may include expressions, formulas, patterns, and types.

This document describes the Ivo language from the bottom up.

- lexical structure
- types
- expressions
- patterns
- definitions
- modules and linking

We give the semantics of Ivo by translation into simpler constructs. This core language consists of just types and a few simple expressions. The formal semantics of the core are defined using PLT Redex and can be found in the appendix. We present a grammar and informal semantics for full Ivo here.

## Lexical structure

### Comments
Comments begin with `//`  and continue to the end of the line.

### Unicode
Ivo supports Unicode-21 identifiers and operators.

### Identifiers and reserved words
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
    where with
    ->
    :
    =
    <-
    :=
	? !
    _

### Literals

#### Integers
Ivo provides several kinds of numbers.
Integer literals can be of any length.
A decimal integer matches the regular expression  `0 | [1-9] [_0-9]*`.
A binary integer matches the regular expression  `0b[_0-1]+`.
A hexadecimal integer matches the regular expression  `0x[_0-9a-fA-F]+`.

#### Rationals
Rational numbers can be written as `decimal . decimal e[+-]? decimal`, or as `hexadecimal . hexadecimal p[+-?] hexadecimal`.
Integer and rational literals are overloaded to support different number
representations.

#### Characters
A character literal is enclosed in single quotes. There are the usual escapes.
Ivo supports Unicode-21 characters.
Character literals are overloaded to support characters in different encodings.

#### Strings
A string literal is enclosed in double quotes. There are the usual escapes.
String literals are overloaded to support different string representations.
Strings can be implemented as lists, arrays, ropes, etc.

### Names

A name is either an identifier or a parenthesized function name.
A function name is a sequence of identifiers and placeholders followed by `=` and another placeholder.
A placeholder is either `_`, indicating an input parameter, or `!`, indicating an output parameter.
For instance, `_ + _ = !` is the name of the forward mode `+` operator, and `_ + ! = _` 
is the name of one of the backward mode + operators.

In any function name, at least one of the placeholders must be `?`.
The forward mode binding placeholder (i.e., `= ?`) can be omitted. So `_ + _` is
another name for the forward mode `+` operator.

### Qualified names

A qualified name consists of a module name (which is just a qualified name)
a `::` and a simple name (as above).

## Types

## Values

Ivo supports the following values:

- primitives of type `#i8`, `#i16`, `#i32`, `#i64`, `#f32`, `#f64`
- function values
- thunks
- references to immutable tagged records

### Functions

Function values take 0 or more parameters and return one value. Function values include a captured environment.
Function values include a mode for the parameters and return.
Function values include an optional flag (`default` or `unique`).

### Thunks

A thunk is a function that takes no parameters.
Thunk values include a captured environment.
Thunk values include an optional flag (`default` or `unique`).

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

Each value literal has a unique tag.

`_` is the any literal. It is equivalent to the union of all values.

`()` is the nil literal. It is equivalent to the empty union.

### Tuple literals

A tuple `(e1, e2, ..., en)` is equivalent to `Prelude::Tuple e1 e2 ... en`.
`n` must be at least 2.

The expression `(e)` is equivalent to `e`.

The expression `()` is the nil literal.

### List literals

The list literal `[e1, e2, e3]` is equivalent to `Prelude::Cons e1 (Prelude::Cons e2 (Prelude::Cons e3 Prelude::Nil))`. `n` may be 0.

### Set literals

The set literal `Set {e1, e2, e3}` is equivalent to `Prelude::Set::fromList [e1, e2, e3]`. `n` may be 0.

### Dictionary literals

The dictionary literal `Dict {e1: e2, e3: e4}` is equivalent to `Prelude::Dict::fromList [(e1, e2), (e3, e4)]`. `n` may be 0.

### Struct literals

The struct literal `T { x1: e1; ...; xn: en }` is a struct with tag `T` and members `xi` initialized to `ei`. `n` may be 0.

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

The expression `e1 | e2` evaluates to the stream containing the values of `e1` and `e2`. All members of the stream must have the same tag. Duplicates are elided.

### Intersection expressions

The expression `e1 & e2` evaluates to the stream containing just `e1` if it is equal to `e2` and to the empty stream `()` otherwise.

### Variables

The expression `x` or `!x` evaluates to the value in variable `x`.

The pattern `x` or `?x` binds variable `x`

### let expressions
A `let` expression tries to satisfy a formula, binding new variables as necessary. For example,

     let 2 + x == 5 {
         print x
     }
  
causes `x` to be bound to `3` in the body, which prints `x`. The value of a `let` expression is the value of the body. Variables bound in the `let` are in scope for the remainder of the enclosing block. `let`-bound variables are immutable.
The formula:

    let x :: xs == [1,2,3] {
        print x
        print xs
    }

binds `x` to `1` and `xs` to the list `[2,3]`.
It is a dynamic error for a formula used in a `let` to generate zero satisfying assignments or more than one satisfying assignment.
The `let` binds any variables in the formula not already bound. If all variables are bound already, the expression must evaluate to `True`.

### if expressions

An `if` expression is like a `let`, but does not generate an error if the formula does not have a solution.

    if 2 + x == 5 {
        print x
    }
An `if` expression can have an optional `else` clause, which is evaluated if there are no solutions to the formula.
The result of an `if` without an `else` is the unit value. The result of an `if` with an `else` is the result of whichever clause is evaluated.

### for expressions

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

### var expressions
These are just like `let` expressions, but define mutable variables. For instance:

	var 2 + x == 5 {
	    x = x * 2  // assigns 6 to x
	    print x    // prints 6
	}

### local definitions expressions

A `let` expression can also declare a function or trait, or a group of mutually recursive functions.

    let fun f (x) = 1 {
        f 2
    }

    let fun odd (0) = False
        fun even (0) = True
        fun odd (n) = even (n-1)
        fun even (n) = odd (n-1) {

		odd 7
	}

    let data Nil
        data Cons (a) (List a) {
        ...
    }
    
### Blocks

A lexical block is enclosed in curly brackets `{}`.

A block expression may contain both expressions and definitions.
The block with nested definitions is equivalent to a telescoping `let`  expression for consecutive definitions declared in the same `let`.

Variables, functions, data types, and traits defined within the block are in scope throughout the block.
These definitions can capture variables from the enclosing scopes.
Definitions in a block may be mutually recursive.
Forward references to non-lazy variables are illegal, however. It is not possible to use a variable in an expression
or a formula before the variable is defined.
If a variable is captured by a function (including within a trait or instance declaration), that function cannot be invoked before the variable is defined.
If a variable is captured by a data (constructor) declaration, that constructor cannot be invoked before the variable is defined.
The value of the block is the value of the last expression in the block.

### Streams
A stream expression generates a possibly infinite sequence of values. There are several ways to define streams, including implementing the `Stream` trait. However, typically a stream can be generated from a formula. 
Given a formula, a stream can be created that iterates through all satisfying assignments of the formula. For instance, the formula `xs contains x && x > 0` iterates through all values in the collection `xs` that are greater than 0.
A stream may be empty, finite, or infinite.
Streams can also be created directly by implementing the `Stream` trait.
A `for` expression can iterate through a stream.
Any unknowns in the formula are bound in the body of the `for` .

### Core expressions

An expression `#(e)` or `#{e}` is a core expression.
A core expression is statically typed. Captured variables are always of type `box`.

	if let x: i32 = unbox e { .. }
	
	unbox e {
	    x: i32 => ...
	    x: i64 => ...
	    x: f32 => ...
	    x: f64 => ...
	    _ => ...
	}

The result of a core expression must be a `box`.

The only core types are `i32`, `i64`, `f32`, `f64`, `i8`, `i16`, and `box`, and function types.

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

## Patterns

Any expression is a pattern matching the value of the expression.

### Wildcard

`_` matches all values.

### Nil

`()` matches no values.

### Record patterns

`T { x1 = p1; ...; xn = pn; xm; _ }` matches records with tag `T` and fields `xi` with patterns `pi` plus the field `xm` with pattern `?xm`, plus any other fields with the wildcard pattern `_`. If the wildcard is omitted, the scrutinee must have exactly those fields.

### Unknowns

The pattern `x` or `?x` binds the variable `x`.

`x` is an unknown if it there is no explicit definition or explicit import (`import M::x` or `import M::(y -> x)` but not `import M::_`) in the enclosing scope.

### Where patterns

`p where f` matches `p` against the scrutinee then matches `f` against `True`. It is equivalent to `p & (f = True)`.

### Union patterns

`p1 | p2` matches both patterns against the scrutinee, matching if either pattern matches. It is a dynamic error if the patterns match different unknowns.

### Intersection patterns

`p1 & p2` matches both patterns against the scrutinee, matching if both patterns match.

### Bind patterns

`p = e` matches `True` and matches the result of `e` against `p`.

### Generator patterns

`p <- e` matches `True` and matches elements of `e` against `p`.

### Apply patterns

The semantics are given by the Redex model.

### Ascribe patterns

`p : T` matches if `p` matches the scrutinee and if the scrutinee has type `T`.

## Data definitions

A `data` definition introduces a data type. Members of the type are tagged records.

The definition may take parameter patterns. Unknowns in the parameter patterns are fields of the record. The name of the definition (and the enclosing definitions) is the tag of the record.

A `data` definition may contain a record with field definitions.
The field definition `_` captures the parameters as `let` definitions.
If the record is missing, the fields are captured from the definition parameters; that is, the default record is `{ _ }`.

The following are equivalent:

    data Nil
    data Nil { }
    data Nil { _ }

    data Cons (hd) (tl)
    data Cons (hd) (tl) { _ }
    data Cons (hd') (tl') { let hd = hd'; let tl = tl' }
    
    data P (a) (b::bs) (c) { fun f (x) = x + c; _ }
    data P (a') (bs') (c') { 
        fun f (x) = x + c; 
        let a = a'; 
        let b::bs = bs';
        let c = c' 
    }

A field definition is either a `let` definition, a `var` definition, or a `fun` definition.

(Nested `trait` and `data` definitions are also allowed?).

A `let` definition consists of the keyword `let` and a formula. The unknowns in the formula become immutable fields of the record.
A `var` definition consists of the keyword `var` and a formula. The unknowns in the formula become mutable fields of the record.

	data Nil             // defines Nil { }
	data Cons (hd) (tl)  // defines Cons { hd, tl }

A `data` definition induces a function definition that matches instances of the type. For instance,

	data Cons (hd) (tl)

induces the following definitions:
  
    fun Cons (? hd) (? tl) = (! Cons { hd, tl })
	fun Cons (! hd) (! tl) = (? Cons { hd, tl })
	fun Cons (! hd) (? tl) = (? Cons { hd, tl })
	fun Cons (? hd) (! tl) = (? Cons { hd, tl })

Each `data` definition induces a record type (constructor) with the same name, for example:

	data Nil  // type Nil { }, or just Nil
	type Nil

	data Cons (hd) (tl) // type Cons { hd: a, tl: b }

	data False // type False	
	data True  // type True

## Type definitions

A `type` definition specifies an alias for a type.

	type Boolean = False | True

`type` definitions can be parameterized:

	type Cons (a) (b) = Cons { hd: a, tl: b }

The following are equivalent (given the `Cons` type constructor above).

	type List (a) = Nil | Cons a (List a)
	type List (a) = Nil | Cons { hd: a, tl: List a }

Like functions and traits, types definitions can be overloaded. The type is equivalent to the union of the types. Thus, the above is equivalent to:

	type List (a) = Nil
	type List (a) = Cons a (List a)

The semantics is that all cases are gathered together into one definition.

A type declared `open` can be overloaded in other modules. Generally, when a type is overloaded in another module, functions involving that type are also overloaded. 

Functions using `open` types should be declared `open`. That is, an `open` type cannot be matched as a parameter of a closed function nor matched using `match` in the body of a closed function. This ensures that the function can be extended to match extensions of the open type. It is allowed for a function to treat an open type parametrically (that is, to move it between variables).

> TODO: Matching of type is checked for completeness within a module. Matching of open types is not checked for completeness.

## Linking 

When linking modules, the modules are checked for ambiguous or incomplete open functions and ambiguous open types.

> TODO: Ambiguous (open) functions are in general allowed in backward modes.

When linking modules, a dispatch function is generated for each open function in the linked modules.

When linking modules, overlapping instances are checked.

It is a compile-time error to have visible overlapping instances.
It is a compile-time error to have visible ambiguous functions and types.
It is a link-time error to have overlapping instances.
It is a link-time error to have ambiguous functions and types.

## Type inference

Each data definition is translated to a data definition.

    data Nil
    data Cons (hd) (tl)

Each type definition is translated into a type class.

	type List (a) = Nil | Cons (a) (List a)

	class xs : List a | xs -> a
	instance Nil : List a Nil
	instance xs : List a => Cons a xs : List a

Use of the type `List a` translates into `List a xs => xs`.

Recursion turns into a type parameter

	type A a = B (A [a])

    class xs : A a | xs -> a
    instance xs : A [a] => B xs : List a

Open function alternatives are just inferred as is. Each alternative may have a different type. It is a link-time error for alternatives to have incompatible types.

### Gradual typing

If a type is omitted, it is inferred. If the inferred type results in a type error, the dynamic type `?` is used. 

When unifying `C` is preferred to `?`, which is preferred to `a`.

## Types

### Core types

We have the core types `#i8`, `#i16`, `#i32`, `#i64`, `#f32`, `#f64`, and `#box`.

### Any type

`_` is the type of any value. 
All types are a subtype of `_`.
It can be considered the infinite union.

### Nothing type

`!` is the type of no value.
All types are a supertype of `!`.
It can be considered the empty union.

### Dynamic type

`?` is the dynamic type. Any value can be coerced to this type.
It is equivalent to `#box`.

### Record types

Any record `Foo { .. xi: ei .. }` has type `Foo { .. xi: ti .. }`.

### Parameterized types

`C t` is a type of `C` is a type constructor (of kind `* -> *`) and `t` is a type. 
As a specific instance, the function type `s -> t` is a type if `s` and `t` are types. 
Also tuple types, list types, etc.

### Type parameters

`a` is a type.

### Union types

`s | t` is the union of types `s` and `t`.
`s` and `t` and both subtypes of `s | t`.

### Intersection types

`s & t` is the intersection of types `s` and `t`.

## Trait definitions

A `trait` definition defines a trait or a trait instance.
If the parameters of the trait are unknowns, the definition is a trait definition.

    trait Monoid (m) {
        fun mempty -> m
        fun mappend (m) (m) -> m
    }

The members of a trait may be abstract.

If the parameters of the trait are not unknowns, they must be types. This defines a trait instance. Members must be non-abstract.

    trait Monoid (List a) {
        fun mempty = []
        fun mappend (xs) (ys) = xs ++ ys
    }

## Types

Each data definition defines a type constructor.
Function types are defined by the built-in class constructor `->`.
Tuple types are defined by the built-in data constructors '(,)', `(,,)`, etc.
The unit type is defined by the built-in data type `()`.

## Function definitions

Functions are declared with the `fun` keyword, followed by zero or more _parameter attributes_ in parentheses. After the parameters is an optional guard `where e`, where `e` is a formula. 
After an `=`, there is the function _return attribute_. The return attribute may be omitted, meaning the function is _abstract_. After the body is another optional `where` clause with a formula.

Attributes may specify a _mode_, either `!` (output) or `?` (input).
If no modes are specified, the parameters are all `?` mode and the return attribute is `!` mode. If a mode is specified for any attribute, it must be specified for all.

Output attributes are expressions. Input attributes are patterns.
When calling a function, input attributes are matched left to right, then the guard is matched against `True`. If all succeed, the function is selected.
If more than one function is selected, the _most specific_ is invoked, if it is unique.

When invoked, the function body `where` clause is evaluated, then the output attributes, in order from left to right.

Functions can be overloaded by providing more than one alternative.
If a function is declared `open`, it can be overloaded in another module.
At link time, the function is checked to determine if the types agree.

### Alternatives

The following function signature is for a function named `inc`, which takes an integer and returns an integer.

    fun inc (x: Int) : Int

The following function signature is for an infix `+` operator on integers.

    fun (_: Int) + (_: Int) : Int

If curly brackets are used instead of parentheses, the argument is call-by-name rather than call-by-value. 

> Call-by-name parameters are not supported in Ivo 1.0.

For example, the following operator defines short-circuiting boolean AND:

    fun (_: Boolean) && {_: Boolean} : Boolean

When invoked, the second argument is not evaluated unless used in the function body.
Functions can be used to define mixfix operators. For example, the ternary function `if` is defined as:

    fun if (Boolean) then {a} else {a} -> a

Parameters may be double-parenthesized (or double-bracketed) to indicate operator associativity. Thus, with the following signature, `+` is left associative:

    fun ((Int)) + (Int) -> Int

With the following signature `::` is right associative:

    fun (_: a) :: ((_: List a)) : List a

At most one argument type can be double-parenthesized.

By default, the first parameter is associative.

Alternatives are selected by best-fit matching.
If a forward context, it is a runtime error for more than one alternative to match.
In a backward context, all matching alternatives are invoked.

Each alternative is given by the function name and patterns for each argument, and then the function body.

	// with types
    fun inc (x: Int): Int = x + 1
    
    // without types
    fun if (True) then {e} else {e} = e
    fun if (False) then {_} else {e} = e
    
## Mixfix function parsing

Mixfix operators may be declared with `fun` definitions. A function is *mixfix* if its signature
includes a symbol identifier or any identifier after the first position.
Each module generates a set of parser rules for the mix-fix functions it defines.
If `E` is the parser nonterminal for expressions
and if `P` is the nonterminal for primary expressions, then
the parser is extended as follows for a module with function definitions `f1`, ... `fn`.

    E  ::= E1
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

### Backward functions

Functions in Ivo can be declared to support several *modes* of evaluation.
Normal evaluation is referred to as *forward mode*. A function can be invoked in a *backward mode* by passing in its return value and optionally some arguments, yielding the other arguments.
A function invoked in a backwards mode can be used as a *pattern* or to define a *stream*.

For example, here is an `inc` function declared in forward mode:

    fun inc (x) = x + 1

Here is the backwards mode:

    fun inc (? Int)  Int
    fun inc (! x) = (? y)
        where x = y - 1

The "function body" (`y`) is just a pattern that matches the return value.
The `where` clause is used to specify the code to evaluate to bind the argument.
Variables in the argument and return value patterns are in scope in the `where` clause.
As shorthand, the returned argument expression can be written inline.

    fun inc (! y - 1) = (? y)

A backward mode can also generate a stream of results. This is done by defining several alternatives which bind the unknown argument variable differently.

    fun (! x) in (? x::xs) = (? True)
    fun (! z) in (? x::xs) = (? True)
      where z in xs
      
Higher-order functions can also be invertible. Here, is the forward mode for `map`, which takes
a function as an argument.

    fun map (_: a -> b) (_: List a): List b
    fun map (f) ([]) = []
    fun map (f) (x::xs) = f x :: map f xs

In the backward mode, the type of the function argument is inverted.

    fun map (f) (! []) = []
    fun map (f) (! z :: map f xs) = (x::xs)
       where f z = x

Using this invertible `map`, we can apply the inverted function to each element of a
list.

    let map inc xs == [1,2,3]
    # binds xs to [0,1,2]

Another example: `zip` and `unzip`:

    // forward
    fun zip xs [] = []
    fun zip [] ys = []
    fun zip (x::xs) (y::ys) = (x,y)::zip xs ys
    // backward
    fun zip (! xs) (! ys) = (? xys)
        where {
            xs = map fst xys
            ys = map snd xys
        }
        
Then we can define `unzip` as:

    fun unzip (xys) = (xs, ys)
      where
        zip xs ys == xys
    

    zipWith:
    # forward mode
    fun zipWith ((a,b) -> c) (List[a]) (List[b]) -> List[c]
    zipWith (f) (xs) [] = []
    zipWith (f) [] (ys) = []
    zipWith (f) (x::xs) (y::ys) = f x y :: zipWith f xs ys
    
    # backward mode
    fun zipWith (c -> (a,b)) (-> List[a]) (-> List[b]) <- List[c]
    fun zipWith (f) (-> xs) (-> ys) = zs
      where {
        xys = map f zs
        xs = map fst xys
        ys = map snd xys
      }

Operations and their inverses:

    fun (a) ** (Nat) -> a where Num a
    fun (x) ** (0) = 1
    fun (x) ** (1) = x
    fun (x) ** (2) = x * x
    fun (x) ** (3) = x * x * x
    fun (x) ** (4) = x * x * x * x
    fun (x) ** (2*n) = {
      let y = x ** n {
          y * y
      }
    }
    fun (x) ** (2*n+1) = x * (x ** 2*n)
    
    fun sqrt (x : Double) = math.sqrt(x)
    fun sqrt (! y * y) = (? y)


## Type system

Ivo implements a version of the Damas-Hindley-Milner type system.
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
    Int8, Int16, Int32, Int64, Int128 
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


## Case expressions

Borrowed from Fortress.

    match planet in _ {
      ["Mercury", "Venus", "Earth", "Mars"] -> "inner"
      ["Jupiter", "Saturn", "Uranus", "Neptune"] -> "outer"
      _ -> "none"
    }

Compare all cases with >

    match max _ _ {
       1 mile -> yes
       1 km -> no
    }


## Imports

An import takes a module name and a selector.

	import M::()              // import no definitions from M
	import M::_               // import all definitions from M
	import M::x               // import only x from M
	import M::(x -> y)        // import only x from M, renaming to y
	import M::(x -> ())       // do not import x from M

The set of names imported from a given path is computed as: 

	all names from M if M::_
	minus
	all names from M if M::()
	minus
    x if M::(x -> ())
    plus
    x if M::x
    plus
    x (as y) if M::(x -> y)

Without a module name, an `import` imports from the enclosing scope. That is, the following are equivalent:

	import x
	import parent::x

## Modules and linking

A module consists of a set of definitions and an expression.
A module can import from other modules.

A program is a set of modules. The main module is given to the interpreter and the expression is evaluated.
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTU2MDUwNjIwNywtMjYzMjk1MCw0MTQwNj
gxMTYsMzcwNDE3MTEwLDE2MzA3OTIxOTYsLTk3NzgwNDAwMiwx
MTcwMTE1MTUsMTQyMjM5MjM5NCw4NDEzMjg1NzcsLTEwNDMwMj
IwMjMsNjA0MzI4Mzc2LC0xOTMwNjM1NzA2LC0xMTg3NzczMTAw
LDIwNjg0OTkzOTQsMTYwOTM0ODIzNSwtMjAyMjIyMjA5MCwtMT
kwNjIzMjI2MCw2Njg2OTI2MjUsMTI2NjMyMjM2MCwxNzE0NTEz
MjU5XX0=
-->