import gleam/string
import gleam/int
import gleam/bytes_builder.{type BytesBuilder, from_bit_array}
import simplifile
import party.{
  alphanum, either, char, digit, do, letter, many, many1, perhaps, return, satisfy,
  string,
}

fn e(bits: BitArray) -> BytesBuilder {
  from_bit_array(bits)
}

fn parameterized(opcode) -> party.Parser(BytesBuilder, Nil) {
  use _ <- do(char(" "))
  use n <- do(
    party.try(many1(digit()), fn(digits) { int.parse(string.concat(digits)) }),
  )
  return(e(<<opcode:8, n:8>>))
}

fn instr() -> party.Parser(BytesBuilder, Nil) {
  use first_char <- do(letter())
  use rest <- do(many(either(alphanum(), char("_"))))
  let s = first_char <> string.concat(rest)
  use bytes <- do(case s {
    "req" -> return(e(<<0>>))
    "region" -> return(e(<<1>>))
    "heap" -> return(e(<<2>>))
    "cap" -> return(e(<<3>>))
    "cap_le" -> return(e(<<4>>))
    "unique" -> return(e(<<5>>))
    "rw" -> return(e(<<6>>))
    "both" -> return(e(<<7>>))
    "handle" -> return(e(<<8>>))
    "i32" -> return(e(<<9>>))
    "end" -> return(e(<<10>>))
    "mut" -> return(e(<<11>>))
    "tuple" -> parameterized(12)
    "arr" -> return(e(<<13>>))
    "all" -> return(e(<<14>>))
    "some" -> return(e(<<15>>))
    "emos" -> return(e(<<16>>))
    "func" -> parameterized(17)
    "ct_get" -> parameterized(18)
    "ct_pop" -> return(e(<<19>>))
    "unpack" -> return(e(<<20>>))
    "get" -> parameterized(21)
    "init" -> parameterized(22)
    "malloc" -> return(e(<<23>>))
    "proj" -> parameterized(24)
    "call" -> return(e(<<25>>))
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

fn parse(s: String) -> BitArray {
  let assert Ok(arrays) = party.go(many1(instr()), s)
  bytes_builder.to_bit_array(
    bytes_builder.concat(arrays)
    |> bytes_builder.prepend(<<0:32>>),
  )
}

pub fn main() {
  let assert Ok(s) = simplifile.read("main.svmt")
  let parsed = parse(s)
  simplifile.write_bits(parsed, to: "bin.svm")
}
