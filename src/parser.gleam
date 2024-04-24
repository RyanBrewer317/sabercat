// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import common.{
  type Expr, type Stmt, type Type, Compose, Exists, Forall, ForallRgn, Func,
  FuncType, Handle, I32, Instr, Lit, Ptr, Stmt, TVar, TupleType, Type,
}
import party.{
  type Parser, alphanum, char, choice, digits, do, either, end, lazy,
  lowercase_letter, many, many1, many_concat, not, perhaps, return, satisfy, sep,
  sep1, string, try, until,
}
import gleam/int
import gleam/list
import gleam/result

fn parse_lit() -> Parser(Expr, Nil) {
  use n <- do(try(digits(), int.parse))
  return(Lit(n))
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

fn parse_type_expr() -> Parser(Expr, Nil) {
  use _ <- do(char("<"))
  use t <- do(parse_type())
  use _ <- do(char(">"))
  return(Type(t))
}

fn parse_expr() -> Parser(Expr, Nil) {
  use <- ws()
  use es <- do(sep1(
    choice([parse_lit(), parse_func(), parse_type_expr(), parse_instr()]),
    by: ws1(),
  ))
  use <- ws()
  case es {
    [] -> panic as "impossible"
    [e] -> return(e)
    [e, ..rest] ->
      list.fold(from: e, over: rest, with: Compose)
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
    Error(Nil) -> return(t)
  }
}

fn parse_stmt() -> Parser(Stmt, Nil) {
  use <- ws()
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
  return(Stmt(name, t, e))
}

pub fn go() -> Parser(List(Stmt), Nil) {
  use stmts <- do(many1(parse_stmt()))
  use _ <- do(end())
  return(stmts)
}
