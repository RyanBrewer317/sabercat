# Sabercat
Sabercat is a low-level concatenative programming language targeting SaberVM. 
Its main purpose is to produce binaries for testing SaberVM. Sabercat is written entirely in Gleam, currently using the BEAM backend.

Here's a little program that prints 5 to stdout:
```
/* comment */
fn main: () -> 0 =
  5 print 0 halt;
```
The `main` function has type `()->0`, meaning it's a function taking no arguments and producing no result. All functions in SaberVM bytecode don't return, because of the enforced Continuation-Passing Style. That means you'll see `->0` a lot, as it merely indicates that something is a function type.

This program puts `5` on the stack, `print`s it, then `halt`s with exit code `0` (that is, exit without error).

