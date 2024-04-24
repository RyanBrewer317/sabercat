// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import common.{
  type Expr, type Stmt, type Type, Type, Compose, Exists, Forall, ForallRgn, Func,
  FuncType, Handle, I32, Instr, Lit, Ptr, Stmt, TVar, TupleType,
}
import gleam/bytes_builder.{type BytesBuilder, from_bit_array}
import gleam/dict.{type Dict}
import gleam/int
import gleam/result.{try}
import gleam/list

pub fn go(stmts: List(Stmt)) -> BytesBuilder {
  
}

fn assemble_expr(e: Expr, funcs: Dict(String, Int), ctsp: Int, ct_vars: Dict(String, Int)) -> Result(BytesBuilder, String) {
  case e {
    Lit(i) -> Ok(op_lit(i))
    Func(name) -> 
      case dict.get(funcs, name) {
        Ok(n) -> Ok(op_global_func(n))
        Error(Nil) -> Error("unknown function `$" <> name <> "`")
      }
    Instr("app") -> Ok(op_app())
    Instr("unpack") -> Ok(op_unpack())
    Instr(instr) -> Error("unknown instruction `" <> instr <> "`")
    Type(t) -> {
      use #(t_asm, _) <- try(assemble_type(t, ctsp, ct_vars))
      Ok(t_asm)
    }
    Compose(f, g) -> {
      use f_asm <- try(assemble_expr(f, funcs, ctsp, ct_vars))
      use g_asm <- try(assemble_expr(g, funcs, ctsp, ct_vars))
      Ok(bytes_builder.append_builder(f_asm, g_asm))
    }
  }
}

fn assemble_type(t: Type, ctsp: Int, ct_vars: Dict(String, Int)) -> Result(#(BytesBuilder, Dict(String, Int)), String) {
  case t {
    I32 -> Ok(op_i32())
    TVar(name) -> 
      case dict.get(ct_vars, name) {
        Ok(stack_pos) -> Ok(op_ct_get(ctsp - stack_pos - 1))
        Error(Nil) -> Error("unknown variable `" <> name <> "`")
      }
    FuncType(ts) -> {
      use #(ts_asm, _) <- try(list.try_fold(from: #(bytes_builder.new(), ctsp), over: ts, with: fn(acc, t) {
        let #(asm, ctsp) = acc
        use #(t_asm, _) <- try(assemble_type(t, ctsp, ct_vars))
        Ok(#(bytes_builder.append_builder(asm, t_asm), ctsp + 1))
      }))
      Ok(bytes_builder.append_builder(ts_asm, op_func(list.length(ts))))
    }
    TupleType(ts) -> {
      use #(ts_asm, _) <- try(list.try_fold(from: #(bytes_builder.new(), ctsp), over: ts, with: fn(acc, t) {
        let #(asm, ctsp) = acc
        use #(t_asm, _) <- try(assemble_type(t, ctsp, ct_vars))
        Ok(#(bytes_builder.append_builder(asm, t_asm), ctsp + 1))
      }))
      Ok(bytes_builder.append_builder(ts_asm, op_tuple(list.length(ts))))
    }
    Ptr(t, r) -> 
      case dict.get(ct_vars, r) {
        Ok(pos) -> {
          use #(t_asm, _) <- try(assemble_type(t, ctsp + 1, ct_vars))
          Ok(bytes_builder(op_ct_get(ctsp - pos - 1), ))
        }
      }
  }
  todo
}

fn op_unique() {
  from_bit_array(<<0:8>>)
}

fn op_handle() {
  from_bit_array(<<1:8>>)
}

fn op_i32() {
  from_bit_array(<<2:8>>)
}

fn op_tuple(n: Int) {
  from_bit_array(<<3:8, n:8>>)
}

fn op_some() {
  from_bit_array(<<4:8>>)
}

fn op_all() {
  from_bit_array(<<5:8>>)
}

fn op_rgn() {
  from_bit_array(<<6:8>>)
}

fn op_end() {
  from_bit_array(<<7:8>>)
}

fn op_app() {
  from_bit_array(<<8:8>>)
}

fn op_func(n: Int) {
  from_bit_array(<<9:8, n:8>>)
}

fn op_ct_get(n: Int) {
  from_bit_array(<<10:8, n:8>>)
}

fn op_lced() {
  from_bit_array(<<11:8>>)
}

fn op_unpack() {
  from_bit_array(<<12:8>>)
}

fn op_get(n: Int) {
  from_bit_array(<<13:8, n:8>>)
}

fn op_init(n: Int) {
  from_bit_array(<<14:8, n: 8>>)
}

fn op_malloc() {
  from_bit_array(<<15:8>>)
}

fn op_proj(n: Int) {
  from_bit_array(<<16:8, n:8>>)
}

fn op_call() {
  from_bit_array(<<17:8>>)
}

fn op_print() {
  from_bit_array(<<18:8>>)
}

fn op_lit(n: Int) {
  let #(a, b, c, d) = bytes(n)
  from_bit_array(<<19:8, d:8, c:8, b:8, a:8>>)
}

fn op_global_func(n: Int) {
  let #(a, b, c, d) = bytes(n)
  from_bit_array(<<20:8, d:8, c:8, b:8, a:8>>)
}

fn op_halt() {
  from_bit_array(<<21:8>>)
}

fn op_pack() {
  from_bit_array(<<22:8>>)
}

fn op_size(n: Int) {
  let #(a, b, c, d) = bytes(n)
  from_bit_array(<<23:8, d:8, c:8, b:8, a:8>>)
}

fn op_new_rgn() {
  from_bit_array(<<24:8>>)
}

fn op_free_rgn() {
  from_bit_array(<<25:8>>)
}

fn op_ptr() {
  from_bit_array(<<26:8>>)
}

fn op_deref() {
  from_bit_array(<<27:8>>)
}

fn bytes(n: Int) -> #(Int, Int, Int, Int) {
  #(
    int.bitwise_shift_right(n, 24),
    int.bitwise_and(int.bitwise_shift_right(n, 16), 0xFF),
    int.bitwise_and(int.bitwise_shift_right(n, 8), 0xFF),
    int.bitwise_and(n, 0xFF),
  )
}