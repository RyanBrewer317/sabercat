// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/string
import gleam/int
import gleam/bytes_builder.{type BytesBuilder, from_bit_array}
import simplifile
import party.{
  type Parser, alphanum, char, digits, do, either, letter, many, many1, perhaps,
  return, satisfy, string,
}

fn e(bits: BitArray) -> Parser(BytesBuilder, e) {
  return(from_bit_array(bits))
}

fn parameterized(op opcode: Int) -> party.Parser(BytesBuilder, Nil) {
  use _ <- do(char(" "))
  use n <- do(party.try(digits(), fn(s) { int.parse(s) }))
  e(<<opcode:8, n:8>>)
}

fn with_lit(op opcode: Int) -> party.Parser(BytesBuilder, Nil) {
  use _ <- do(char(" "))
  use n <- do(party.try(digits(), fn(s) { int.parse(s) }))
  let #(a, b, c, d) = bytes(n)
  e(<<opcode:8, d:8, c:8, b:8, a:8>>)
}

fn bytes(n: Int) -> #(Int, Int, Int, Int) {
  #(
    int.bitwise_shift_right(n, 24),
    int.bitwise_and(int.bitwise_shift_right(n, 16), 0xFF),
    int.bitwise_and(int.bitwise_shift_right(n, 8), 0xFF),
    int.bitwise_and(n, 0xFF),
  )
}

fn instr() -> party.Parser(BytesBuilder, Nil) {
  use first_char <- do(letter())
  use rest <- do(many(either(alphanum(), char("_"))))
  let s = first_char <> string.concat(rest)
  use bytes <- do(case s {
    "unique" -> e(<<0>>)
    "handle" -> e(<<1>>)
    "i32" -> e(<<2>>)
    "tuple" -> parameterized(op: 3)
    "some" -> e(<<4>>)
    "all" -> e(<<5>>)
    "rgn" -> e(<<6>>)
    "end" -> e(<<7>>)
    "app" -> e(<<8>>)
    "func" -> parameterized(op: 9)
    "ct_get" -> parameterized(op: 10)
    "lced" -> e(<<11>>)
    "unpack" -> e(<<12>>)
    "get" -> parameterized(13)
    "init" -> parameterized(14)
    "malloc" -> e(<<15>>)
    "proj" -> parameterized(16)
    "call" -> e(<<17>>)
    "print" -> e(<<18>>)
    "lit" -> with_lit(op: 19)
    "global_func" -> with_lit(op: 20)
    "halt" -> e(<<21>>)
    "pack" -> e(<<22>>)
    "size" -> with_lit(op: 23)
    "new_rgn" -> e(<<24>>)
    "free_rgn" -> e(<<25>>)
    "ptr" -> e(<<26>>)
    "deref" -> e(<<27>>)
    s -> panic as { "unknown instruction " <> s }
  })
  use _ <- do(many(either(char(" "), char("\t"))))
  use _ <- do(
    perhaps({
      use _ <- do(string("//"))
      many(satisfy(fn(c) { c != "\n" }))
    }),
  )
  use _ <- do(perhaps(char("\n")))
  return(bytes)
}

fn parse() -> Parser(BitArray, Nil) {
  use n <- do(party.try(digits(), fn(s) { int.parse(s) }))
  let #(a, b, c, d) = bytes(n)
  use _ <- do(party.whitespace())
  use instrs <- do(many1(instr()))
  bytes_builder.concat(instrs)
  |> bytes_builder.prepend(<<0:32, d:8, c:8, b:8, a:8>>)
  |> bytes_builder.to_bit_array()
  |> return()
}

pub fn main() {
  let assert Ok(s) = simplifile.read("main.svmt")
  let assert Ok(parsed) = party.go(parse(), s)
  simplifile.write_bits(parsed, to: "bin.svm")
}
