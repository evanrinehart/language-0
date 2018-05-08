# Language Level 0

Implementation of the simplest possible functional language, untyped lambda
calculus with constants and simple let.

This is the full grammar:

```
e := c | x | e e | \x -> e | let x = e in e
```

Constants are non-negative numbers like 3 or 99.99

Uses Megaparsec to parse code.

Contains a parser, a pretty printer, an evaluator, and a executable wrapper.

To compile:

```
ghc Main
```

To run:

```
echo "3.14" | ./Main myCode
```

If myCode evaluates to a function, the function will be applied to the number
given on stdin. Otherwise whatever it evaluates to will be printed out.

If there is a syntax error in your code, an error report will be printed to
stderr.
