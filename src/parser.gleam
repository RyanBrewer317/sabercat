// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import common.{
  type Expr, type Stmt, type Type, Array, CTAssignment, Compose, Exists, Forall,
  ForallRgn, Func, FuncType, Handle, I32, Instr, I32Lit, U8Lit, Ptr, Stmt, TVar, TupleType,
  Type, U8,
}
import gleam/int
import gleam/list
import gleam/result
import party.{
  type Parser, alphanum, char, choice, digits, do, either, end, lazy,
  lowercase_letter, many, many1, many_concat, not, perhaps, return, satisfy, sep,
  sep1, string, try, until, seq
}

fn parse_lit() -> Parser(Expr, Nil) {
  use res <- do(perhaps(char("-")))
  let coeff = case res {
    Ok(_) -> -1
    Error(Nil) -> 1
  }
  use n <- do(try(digits(), int.parse))
  use res <- do(perhaps(string("u8")))
  case res {
    Ok(_) -> return(U8Lit(coeff * n))
    Error(Nil) -> return(I32Lit(coeff * n))
  }
}

fn word() -> Parser(String, e) {
  use first <- do(lowercase_letter())
  use rest <- do(many_concat(either(alphanum(), char("_"))))
  return(first <> rest)
}

fn parse_func() -> Parser(Expr, e) {
  use _ <- do(char("$"))
  use name <- do(word())
  return(Func(name))
}

fn parse_instr() -> Parser(Expr, e) {
  party.map(word(), Instr)
}

fn parse_ct_assignment() -> Parser(Expr, e) {
  use _ <- do(char("="))
  use name <- do(word())
  return(CTAssignment(name))
}

fn parse_type_expr() -> Parser(Expr, Nil) {
  use _ <- do(char("<"))
  use t <- do(parse_type())
  use _ <- do(char(">"))
  return(Type(t))
}

fn parse_expr() -> Parser(Expr, Nil) {
  use <- ws()
  use es <- do(sep1(
    choice([
      parse_lit(),
      parse_func(),
      parse_type_expr(),
      parse_ct_assignment(),
      parse_instr(),
    ]),
    by: ws1(),
  ))
  use <- ws()
  case list.reverse(es) {
    [] -> panic as "impossible"
    [e] -> return(e)
    [last, ..rest] ->
      list.fold(from: last, over: rest, with: fn(acc, e) { Compose(e, acc) })
      |> return
  }
}

fn parse_tvar() -> Parser(Type, e) {
  party.map(word(), TVar)
}

fn parse_i32_type() -> Parser(Type, e) {
  use _ <- do(string("i32"))
  return(I32)
}

fn parse_u8_type() -> Parser(Type, e) {
  use _ <- do(string("u8"))
  return(U8)
}

fn parenthetical_t() -> Parser(Type, Nil) {
  use _ <- do(char("("))
  use <- ws()
  use ts <- do(sep(lazy(parse_type), by: char(",")))
  use <- ws()
  use _ <- do(char(")"))
  use <- ws()
  use res <- do(perhaps(string("->")))
  case res {
    Ok(_) -> {
      use <- ws()
      use _ <- do(char("0"))
      return(FuncType(ts))
    }
    Error(Nil) -> {
      return(TupleType(ts))
    }
  }
}

fn parse_forall() -> Parser(Type, Nil) {
  use _ <- do(string("Forall"))
  use _ <- do(not(either(alphanum(), char("_"))))
  use <- ws()
  use var <- do(word())
  use <- ws()
  use _ <- do(char(":"))
  use <- ws()
  use res <- do(perhaps(string("Rgn")))
  case res {
    Ok(_) -> {
      use res <- do(perhaps(char("!")))
      let unique = result.is_ok(res)
      use <- ws()
      use _ <- do(char("."))
      use body <- do(lazy(parse_type))
      return(ForallRgn(var, unique, body))
    }
    Error(Nil) -> {
      use size <- do(try(digits(), int.parse))
      use _ <- do(string("byte"))
      use <- ws()
      use _ <- do(char("."))
      use body <- do(lazy(parse_type))
      return(Forall(var, size, body))
    }
  }
}

fn parse_exists() -> Parser(Type, Nil) {
  use _ <- do(string("Exists"))
  use _ <- do(not(either(alphanum(), char("_"))))
  use <- ws()
  use var <- do(word())
  use <- ws()
  use _ <- do(char(":"))
  use <- ws()
  use size <- do(try(digits(), int.parse))
  use _ <- do(string("byte"))
  use <- ws()
  use _ <- do(char("."))
  use body <- do(lazy(parse_type))
  return(Exists(var, size, body))
}

fn parse_handle() -> Parser(Type, e) {
  use _ <- do(string("handle"))
  use <- ws()
  use _ <- do(char("("))
  use <- ws()
  use r <- do(word())
  use <- ws()
  use _ <- do(char(")"))
  return(Handle(r))
}

fn parse_comment() -> Parser(String, e) {
  use _ <- do(string("/*"))
  use _ <- do(until(do: satisfy(fn(_) { True }), until: string("*/")))
  return("")
}

fn ws(k: fn() -> Parser(a, e)) -> Parser(a, e) {
  use _ <- do(
    many(choice([char(" "), char("\t"), char("\n"), parse_comment()])),
  )
  k()
}

fn ws1() -> Parser(Nil, e) {
  use _ <- do(
    many1(choice([char(" "), char("\t"), char("\n"), parse_comment()])),
  )
  return(Nil)
}

fn parse_type() -> Parser(Type, Nil) {
  use <- ws()
  use t <- do(
    choice([
      parse_i32_type(),
      parse_u8_type(),
      parenthetical_t(),
      parse_forall(),
      parse_exists(),
      parse_handle(),
      parse_tvar(),
    ]),
  )
  use <- ws()
  use res <- do(perhaps(char("@")))
  case res {
    Ok(_) -> {
      use <- ws()
      use r <- do(word())
      use <- ws()
      return(Ptr(t, r))
    }
    Error(Nil) -> {
      use res <- do(perhaps(string("[]")))
      case res {
        Ok(_) -> {
          use <- ws()
          use _ <- do(char("@"))
          use <- ws()
          use r <- do(word())
          use <- ws()
          return(Array(t, r))
        }
        Error(Nil) -> return(t)
      }
    }
  }
}

fn parse_import() -> Parser(#(String, Type), Nil) {
  use <- ws()
  use _ <- do(string("import"))
  use _ <- do(not(either(alphanum(), char("_"))))
  use <- ws()
  use name <- do(word())
  use <- ws()
  use _ <- do(char(":"))
  use t <- do(parse_type())
  use _ <- do(char(";"))
  use <- ws()
  return(#(name, t))
}

fn parse_imports() -> Parser(List(#(String, Type)), Nil) {
  many(parse_import())
}

fn parse_stmt() -> Parser(Stmt, Nil) {
  use <- ws()
  use res <- do(perhaps(seq(string("pub"), ws1())))
  let exported = result.is_ok(res)
  use _ <- do(string("fn"))
  use _ <- do(not(either(alphanum(), char("_"))))
  use <- ws()
  use name <- do(word())
  use <- ws()
  use _ <- do(char(":"))
  use t <- do(parse_type())
  use _ <- do(char("="))
  use e <- do(parse_expr())
  use _ <- do(char(";"))
  use <- ws()
  return(Stmt(name, exported, t, e))
}

fn parse_data_section() -> Parser(List(Int), Nil) {
  use <- ws()
  use res <- do(perhaps(string("data:")))
  case res {
    Ok(_) -> {
      use <- ws()
      use _ <- do(char("["))
      use bytes <- do(sep(party.try(digits(), int.parse), by: ws1()))
      use _ <- do(char("]"))
      return(bytes)
    }
    Error(Nil) -> return([])
  }
}

pub fn go() -> Parser(#(List(Int), List(#(String, Type)), List(Stmt)), Nil) {
  use data_section <- do(parse_data_section())
  use imports <- do(parse_imports())
  use stmts <- do(many1(parse_stmt()))
  use _ <- do(end())
  return(#(data_section, imports, stmts))
}
