# Sabercat
Sabercat is a low-level concatenative programming language targeting SaberVM. 
Its main purpose is to produce binaries for testing SaberVM. Sabercat is written entirely in Gleam, currently using the BEAM backend.

Here's a little program that prints 5 to stdout:
```
/* comment */
fn main: () -> 0 =
  5 print 0 halt;
```
- The `main` function has type `()->0`, meaning it's a function taking no arguments and producing no result.
  - All functions in SaberVM bytecode don't return, because of the enforced Continuation-Passing Style. That means you'll see `->0` a lot, as it merely indicates that something is a function type.
- This program puts `5` on the stack, `print`s it, then `halt`s with exit code `0` (that is, exit without error).

Now that you've got your feet wet, here's a longer example:
```
/* allocate a region, call `user` with the continuation `destroyer` */
fn main: ()->0 = 
    20 new_rgn =r $destroyer <r> app $user call;

/*
 * put a 1-element tuple in the given region, put 3 in the tuple, 
 * get 3 out, print 3, call continuation 
 */
fn user: Forall r: Rgn. (handle(r), (handle(r))->0)->0 =
    1 get <(i32)@r> malloc 3 0 init 0 proj print call;

/* free the given region and halt the program normally */
fn destroyer: Forall r: Rgn!. (handle(r))->0 =
    free_rgn 0 halt;
```
This program puts `3` in a heap-allocated tuple, reads it back out onto the stack, prints it to stdout, and lastly frees the memory it used and exits normally. Some notes:
- `new_rgn` creates a *region* (think big chunk of memory), pushes the pointer to it onto the stack, and pushes a compile-time reference to the region on the *compile-time stack.* In this case the region will have 20 bytes of memory; the minimum for a single tuple holding just an `i32`.
- `=r` creates an identifier, `r`, for talking about the thing currently on top of the compile-time stack, in this case the new region.
- `$` puts a function on the stack, in this case `destroyer` (functions are first-class but don't capture anything; it's like in C).
- `<>` puts a compile-time thing on the compile-time stack, in this case a duplicate of the new region reference `r`.
- `app` pops `destroyer` and `r` and instantiates `destroyer`'s region variable with `r`. This works because `destroyer`'s type starts with `Forall r: Rgn`.
  - Because of the exclamation point (`!`) in `destroyer`'s type, `destroyer` must taken "ownership" of the region, that is, the instantiated region must refer to a region that none of `destroyer`'s other region variables refer to, allowing the region to be safely freed. In this case that's trivial, since `destroyer` only takes one region, but this often isn't the case, so owning functions generally have to be manually `app`'d right after a region is created, as that's the main way to know that no other region variables in scope refer to that region.
- `call` pops a function and its arguments off the stack and calls the function with those arguments. In the first case, it pops `user` off the stack, as well as its arguments, which are the region pointer and `destroyer`.
- `handle(r)` is the type of pointers to the region `r`. This is a singleton type, meaning there may be multiple values of type `handle(r)` but they are all equal according to some theoretical `==` operator.
- `get` copies something anywhere in the stack, putting the copy at the top of the stack.
  - Its index, in this case `1`, has to be compile-time known, for typechecking purposes, so Sabercat restricts `get` to only have a literal in front of it. The same restriction applies to `init` and `proj`.
- `(i32)@r` is the type of a pointer to a one-element tuple containing a 32-bit integer. The tuple is allocated in `r`. (Notice how we can use the variables of the function's type signature!)
  - SaberVM also supports unboxed tuples, which in this case would just be written `(i32)`. Tuple-handling ops (`malloc`, `init`, `proj`) are overloaded to handle both stack-allocated and region-allocated tuples, even though they're different types. For type purposes, think of `(i32)@r` like `(i32)*`, ie a pointer to a `(i32)`. Indeed, Sabercat has a `deref` instruction for copying a heap-allocated tuple onto the stack.
- `malloc` pops a type (of a tuple or a pointer to a tuple) off the compile-time stack, and a region pointer off the stack, and pushes either a buffer for the new tuple, or a pointer to a heap-allocated buffer. The type system ensures these buffers are unreadable except sections that have been written to, so `malloc` doesn't need to zero-initialize them.
- `init` pops a tuple (or a pointer to a tuple) which I'll call `tpl`, a value which I'll call `v`, and a compile-time known integer literal which I'll call `i`, and writes `v` to the `i`th component of `tpl`, if the types match up. If `tpl` is allocated in a region, the type system guarantees that the region hasn't been freed. The tuple itself may have been freed though, in which case a runtime exception is thrown. Note that the `i`th tuple component can't have been previously initialized; `init` pushes the "transformed" tuple to the stack as if the operation is not in-place (but it is), with new compile-time information that the written component is now readable and not writable, instead of writable and not readable. Later I'll add mutable values to SaberVM but currently there are none.
- `proj` pops a tuple and a compile-time-known integer literal (which I'll call `i`) and pushes the `i`th component of the tuple onto the stack. That component must have been previously initialized with `init`.
- `print` pops an `i32` off the stack and writes it to stdout, with a newline after it.
- `free_rgn` pops a compile-time region reference off the stack, in this case the `r` of the type signature, and also pops a pointer to that region off the stack. It frees the region and makes it impossible for subsequent code to refer to the freed region.
