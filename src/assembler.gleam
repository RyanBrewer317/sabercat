// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import common.{
  type Expr, type Stmt, type Type, Array, CTAssignment, Compose, Exists, Forall,
  ForallRgn, Func, FuncType, Handle, I32, Instr, Lit, Ptr, Stmt, TVar, TupleType,
  Type, U8,
}
import gleam/bytes_builder.{
  type BytesBuilder, append_builder, from_bit_array, to_bit_array,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result.{try}

pub fn go(
  data_section: List(Int),
  stmts: List(Stmt),
) -> Result(BitArray, String) {
  let data_section_builder =
    bytes_builder.concat(
      list.map(data_section, fn(i) { from_bit_array(<<i:8>>) }),
    )
  let funcs =
    list.index_fold(stmts, dict.new(), fn(acc, stmt, i) {
      dict.insert(acc, stmt.name, i)
    })
  use l <- try(list.try_map(stmts, assemble_stmt(_, funcs)))
  let #(ts_asm_l, defns_asm_l) = list.unzip(l)
  let ts_asm =
    bytes_builder.concat(list.map(ts_asm_l, append_builder(_, op_lced())))
  let defns_asm = bytes_builder.concat(defns_asm_l)
  let #(a, b, c, d) = bytes(list.length(stmts))
  let #(w, x, y, z) = bytes(list.length(data_section))
  <<z:8, y:8, x:8, w:8>>
  |> from_bit_array()
  |> append_builder(data_section_builder)
  |> bytes_builder.append(<<d:8, c:8, b:8, a:8>>)
  |> append_builder(ts_asm)
  |> append_builder(defns_asm)
  |> to_bit_array
  |> Ok
}

fn assemble_stmt(
  stmt: Stmt,
  funcs: Dict(String, Int),
) -> Result(#(BytesBuilder, BytesBuilder), String) {
  let Stmt(_name, t, body) = stmt
  use t_asm <- try(assemble_type(t, 0, dict.new()))
  let #(starting_ct_stack, ctsp) = get_starting_ct_stack(t, 0)
  use body_asm <- try(assemble_expr(body, funcs, ctsp, starting_ct_stack))
  Ok(#(t_asm, body_asm))
}

fn assemble_expr(
  e: Expr,
  funcs: Dict(String, Int),
  ctsp: Int,
  ct_vars: Dict(String, Int),
) -> Result(BytesBuilder, String) {
  case e {
    Lit(i) -> Ok(op_lit(i))
    Func(name) ->
      case dict.get(funcs, name) {
        Ok(n) -> Ok(op_global_func(n))
        Error(Nil) -> Error("unknown function `$" <> name <> "`")
      }
    Instr("app") -> Ok(op_app())
    Instr("unpack") -> Ok(op_unpack())
    Instr("malloc") -> Ok(op_malloc())
    Instr("call") -> Ok(op_call())
    Instr("print") -> Ok(op_print())
    Instr("halt") -> Ok(op_halt())
    Instr("pack") -> Ok(op_pack())
    Instr("free_rgn") -> Ok(op_free_rgn())
    Instr("deref") -> Ok(op_deref())
    Instr("arr_init") -> Ok(op_arr_init())
    Instr("arr_proj") -> Ok(op_arr_proj())
    Instr("addi32") -> Ok(op_add_i32())
    Instr("muli32") -> Ok(op_mul_i32())
    Instr("divi32") -> Ok(op_div_i32())
    Instr("callnz") -> Ok(op_call_nz())
    Instr("print_n") -> Ok(op_print_n())
    Instr(instr) -> Error("unknown instruction `" <> instr <> "`")
    Type(t) -> {
      use t_asm <- try(assemble_type(t, ctsp, ct_vars))
      Ok(t_asm)
    }
    CTAssignment(_var) -> panic as "hi"
    Compose(Lit(n), Instr("get")) -> Ok(op_get(n))
    Compose(Lit(n), Compose(Instr("get"), h)) -> {
      use h_asm <- try(assemble_expr(h, funcs, ctsp, ct_vars))
      Ok(append_builder(op_get(n), h_asm))
    }
    Compose(Lit(n), Instr("init")) -> Ok(op_init(n))
    Compose(Lit(n), Compose(Instr("init"), h)) -> {
      use h_asm <- try(assemble_expr(h, funcs, ctsp, ct_vars))
      Ok(append_builder(op_init(n), h_asm))
    }
    Compose(Lit(n), Instr("proj")) -> Ok(op_proj(n))
    Compose(Lit(n), Compose(Instr("proj"), h)) -> {
      use h_asm <- try(assemble_expr(h, funcs, ctsp, ct_vars))
      Ok(append_builder(op_proj(n), h_asm))
    }
    Compose(Lit(n), Instr("new_rgn")) -> Ok(op_new_rgn(n))
    Compose(Lit(n), Compose(Instr("new_rgn"), g)) -> {
      use g_asm <- try(assemble_expr(g, funcs, ctsp + 1, ct_vars))
      Ok(append_builder(op_new_rgn(n), g_asm))
    }
    Compose(Lit(n), Instr("data")) -> Ok(op_data(n))
    Compose(Lit(n), Compose(Instr("data"), g)) -> {
      use g_asm <- try(assemble_expr(g, funcs, ctsp, ct_vars))
      Ok(append_builder(op_data(n), g_asm))
    }
    Compose(CTAssignment(var), g) ->
      assemble_expr(g, funcs, ctsp, dict.insert(ct_vars, var, ctsp - 1))
    Compose(_f, CTAssignment(_var)) -> panic as "lo"
    Compose(f, g) -> {
      use f_asm <- try(assemble_expr(f, funcs, ctsp, ct_vars))
      use g_asm <- try(assemble_expr(g, funcs, ctsp, ct_vars))
      Ok(append_builder(f_asm, g_asm))
    }
  }
}

fn get_starting_ct_stack(t: Type, ctsp: Int) -> #(Dict(String, Int), Int) {
  case t {
    Forall(var, _size, body) -> {
      let #(ct_stack, out_ctsp) = get_starting_ct_stack(body, ctsp + 1)
      #(dict.insert(ct_stack, var, ctsp), out_ctsp)
    }
    ForallRgn(r, _unique, body) -> {
      let #(ct_stack, out_ctsp) = get_starting_ct_stack(body, ctsp + 1)
      #(dict.insert(ct_stack, r, ctsp), out_ctsp)
    }
    _ -> #(dict.new(), ctsp)
  }
}

fn assemble_type(
  t: Type,
  ctsp: Int,
  ct_vars: Dict(String, Int),
) -> Result(BytesBuilder, String) {
  case t {
    I32 -> Ok(op_i32())
    U8 -> Ok(op_u8())
    TVar(name) ->
      case dict.get(ct_vars, name) {
        Ok(stack_pos) -> Ok(op_ct_get(ctsp - stack_pos - 1))
        Error(Nil) -> Error("unknown variable `" <> name <> "`")
      }
    FuncType(ts) -> {
      use #(ts_asm, _) <- try(
        list.try_fold(
          from: #(bytes_builder.new(), ctsp),
          over: ts,
          with: fn(acc, t) {
            let #(asm, ctsp) = acc
            use t_asm <- try(assemble_type(t, ctsp, ct_vars))
            Ok(#(append_builder(asm, t_asm), ctsp + 1))
          },
        ),
      )
      Ok(append_builder(ts_asm, op_func(list.length(ts))))
    }
    TupleType(ts) -> {
      use #(ts_asm, _) <- try(
        list.try_fold(
          from: #(bytes_builder.new(), ctsp),
          over: ts,
          with: fn(acc, t) {
            let #(asm, ctsp) = acc
            use t_asm <- try(assemble_type(t, ctsp, ct_vars))
            Ok(#(append_builder(asm, t_asm), ctsp + 1))
          },
        ),
      )
      Ok(append_builder(ts_asm, op_tuple(list.length(ts))))
    }
    Ptr(t, "data_section") -> {
      use t_asm <- try(assemble_type(t, ctsp + 1, ct_vars))
      op_data_sec()
      |> append_builder(t_asm)
      |> append_builder(op_ptr())
      |> Ok
    }
    Ptr(t, r) ->
      case dict.get(ct_vars, r) {
        Ok(pos) -> {
          use t_asm <- try(assemble_type(t, ctsp + 1, ct_vars))
          op_ct_get(ctsp - pos - 1)
          |> append_builder(t_asm)
          |> append_builder(op_ptr())
          |> Ok
        }
        Error(Nil) -> Error("unknown region variable `" <> r <> "`")
      }
    Forall(var, size, body) -> {
      use body_asm <- try(assemble_type(
        body,
        ctsp + 1,
        dict.insert(ct_vars, var, ctsp),
      ))
      op_size(size)
      |> append_builder(op_all())
      |> append_builder(body_asm)
      |> append_builder(op_end())
      |> Ok
    }
    ForallRgn(r, unique, t) -> {
      use t_asm <- try(assemble_type(t, ctsp + 1, dict.insert(ct_vars, r, ctsp)))
      let unique_asm = case unique {
        True -> op_unique()
        False -> bytes_builder.new()
      }
      unique_asm
      |> append_builder(op_rgn())
      |> append_builder(t_asm)
      |> append_builder(op_end())
      |> Ok
    }
    Exists(var, size, body) -> {
      use body_asm <- try(assemble_type(
        body,
        ctsp + 1,
        dict.insert(ct_vars, var, ctsp),
      ))
      op_size(size)
      |> append_builder(op_some())
      |> append_builder(body_asm)
      |> append_builder(op_end())
      |> Ok
    }
    Handle("data_section") -> Error("data section doesn't have a handler")
    Handle(r) ->
      case dict.get(ct_vars, r) {
        Ok(pos) -> Ok(append_builder(op_ct_get(ctsp - pos - 1), op_handle()))
        Error(Nil) -> Error("unknown region `" <> r <> "`")
      }
    Array(t, "data_section") -> {
      use t_asm <- try(assemble_type(t, ctsp + 1, ct_vars))
      op_data_sec()
      |> append_builder(t_asm)
      |> append_builder(op_arr())
      |> Ok
    }
    Array(t, r) ->
      case dict.get(ct_vars, r) {
        Ok(pos) -> {
          use t_asm <- try(assemble_type(t, ctsp + 1, ct_vars))
          op_ct_get(ctsp - pos - 1)
          |> append_builder(t_asm)
          |> append_builder(op_arr())
          |> Ok
        }
        Error(Nil) -> Error("unknown region `" <> r <> "`")
      }
  }
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
  from_bit_array(<<14:8, n:8>>)
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

fn op_new_rgn(n: Int) {
  let #(a, b, c, d) = bytes(n)
  from_bit_array(<<24:8, d:8, c:8, b:8, a:8>>)
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

fn op_arr() {
  from_bit_array(<<28:8>>)
}

fn op_arr_init() {
  from_bit_array(<<29:8>>)
}

fn op_arr_proj() {
  from_bit_array(<<30:8>>)
}

fn op_add_i32() {
  from_bit_array(<<31:8>>)
}

fn op_mul_i32() {
  from_bit_array(<<32:8>>)
}

fn op_div_i32() {
  from_bit_array(<<33:8>>)
}

fn op_call_nz() {
  from_bit_array(<<34:8>>)
}

fn op_data(n: Int) {
  let #(a, b, c, d) = bytes(n)
  from_bit_array(<<35:8, d:8, c:8, b:8, a:8>>)
}

fn op_data_sec() {
  from_bit_array(<<36:8>>)
}

fn op_u8() {
  from_bit_array(<<37:8>>)
}

fn op_print_n() {
  from_bit_array(<<38:8>>)
}

fn bytes(n: Int) -> #(Int, Int, Int, Int) {
  #(
    int.bitwise_shift_right(n, 24),
    int.bitwise_and(int.bitwise_shift_right(n, 16), 0xFF),
    int.bitwise_and(int.bitwise_shift_right(n, 8), 0xFF),
    int.bitwise_and(n, 0xFF),
  )
}
