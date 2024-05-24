// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import common.{
  type Expr, type Stmt, type Type, Array, CTAssignment, Compose, Exists, Forall,
  ForallRgn, Func, FuncType, Handle, I32, I32Lit, Instr, Ptr, Stmt, TVar,
  TupleType, Type, U8, U8Lit,
}
import gleam/bytes_builder.{
  type BytesBuilder, append_builder, from_bit_array, to_bit_array,
}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result.{try}
import gleam/string

pub fn go(
  data_section: List(Int),
  imports: List(#(String, Type)),
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
  let num_stmts = list.length(stmts)
  let funcs =
    list.index_fold(imports, funcs, fn(acc, impt, i) {
      dict.insert(acc, impt.0, num_stmts + i)
    })
  use l <- try(list.try_map(stmts, assemble_stmt(_, funcs)))
  let #(ts_asm_l, defns_asm_l) = list.unzip(l)
  let ts_asm =
    bytes_builder.concat(
      list.map(ts_asm_l, fn(pair) {
        let #(t_asm, stmt) = pair
        case stmt.exported {
          True -> {
            let id = str_to_id(stmt.name)
            append_builder(t_asm, op_export(id.0, id.1, id.2, id.3))
          }
          False -> append_builder(t_asm, op_lced())
        }
      }),
    )
  use imports_asms <- try(
    list.try_map(imports, fn(impt) {
      use t_asm <- try(assemble_type(impt.1, 0, dict.new()))
      let id = str_to_id(impt.0)
      append_builder(t_asm, op_import(id.0, id.1, id.2, id.3))
      |> Ok
    }),
  )
  let ts_asm = append_builder(ts_asm, bytes_builder.concat(imports_asms))
  let defns_asm = bytes_builder.concat(defns_asm_l)
  let #(a, b, c, d) = bytes(list.length(stmts) + list.length(imports))
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
) -> Result(#(#(BytesBuilder, Stmt), BytesBuilder), String) {
  let Stmt(_name, _, t, body) = stmt
  use t_asm <- try(assemble_type(t, 0, dict.new()))
  let #(starting_ct_stack, ctsp) = get_starting_ct_stack(t, 0)
  use body_asm <- try(assemble_expr(body, funcs, ctsp, starting_ct_stack))
  Ok(#(#(t_asm, stmt), body_asm))
}

fn assemble_expr(
  e: Expr,
  funcs: Dict(String, Int),
  ctsp: Int,
  ct_vars: Dict(String, Int),
) -> Result(BytesBuilder, String) {
  case e {
    I32Lit(i) -> Ok(op_lit(i))
    U8Lit(i) -> Ok(op_u8_lit(i))
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
    Instr("arr_mut") -> Ok(op_arr_mut())
    Instr("arr_proj") -> Ok(op_arr_proj())
    Instr("add") -> Ok(op_add())
    Instr("mul") -> Ok(op_mul())
    Instr("div") -> Ok(op_div())
    Instr("callnz") -> Ok(op_call_nz())
    Instr("copy_n") -> Ok(op_copy_n())
    Instr("u8_to_i32") -> Ok(op_u8_to_i32())
    Instr(instr) -> Error("unknown instruction `" <> instr <> "`")
    Type(t) -> {
      use t_asm <- try(assemble_type(t, ctsp, ct_vars))
      Ok(t_asm)
    }
    CTAssignment(_var) -> panic as "hi"
    Compose(I32Lit(n), Instr("get")) -> Ok(op_get(n))
    Compose(I32Lit(n), Compose(Instr("get"), h)) -> {
      use h_asm <- try(assemble_expr(h, funcs, ctsp, ct_vars))
      Ok(append_builder(op_get(n), h_asm))
    }
    Compose(I32Lit(n), Instr("init")) -> Ok(op_init(n))
    Compose(I32Lit(n), Compose(Instr("init"), h)) -> {
      use h_asm <- try(assemble_expr(h, funcs, ctsp, ct_vars))
      Ok(append_builder(op_init(n), h_asm))
    }
    Compose(I32Lit(n), Instr("proj")) -> Ok(op_proj(n))
    Compose(I32Lit(n), Compose(Instr("proj"), h)) -> {
      use h_asm <- try(assemble_expr(h, funcs, ctsp, ct_vars))
      Ok(append_builder(op_proj(n), h_asm))
    }
    Compose(I32Lit(n), Instr("new_rgn")) -> Ok(op_new_rgn(n))
    Compose(I32Lit(n), Compose(Instr("new_rgn"), g)) -> {
      use g_asm <- try(assemble_expr(g, funcs, ctsp + 1, ct_vars))
      Ok(append_builder(op_new_rgn(n), g_asm))
    }
    Compose(I32Lit(n), Instr("data")) -> Ok(op_data(n))
    Compose(I32Lit(n), Compose(Instr("data"), g)) -> {
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
    I32 -> Ok(op())
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

fn op() {
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

fn op_arr_mut() {
  from_bit_array(<<29:8>>)
}

fn op_arr_proj() {
  from_bit_array(<<30:8>>)
}

fn op_add() {
  from_bit_array(<<31:8>>)
}

fn op_mul() {
  from_bit_array(<<32:8>>)
}

fn op_div() {
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

fn op_copy_n() {
  from_bit_array(<<38:8>>)
}

fn op_u8_lit(n) {
  from_bit_array(<<39:8, n:8>>)
}

fn op_u8_to_i32() {
  from_bit_array(<<40:8>>)
}

fn op_import(x: Int, y: Int, z: Int, w: Int) {
  let #(ax, bx, cx, dx) = bytes(x)
  let #(ay, by, cy, dy) = bytes(y)
  let #(az, bz, cz, dz) = bytes(z)
  let #(aw, bw, cw, dw) = bytes(w)
  from_bit_array(<<
    41:8,
    dw:8,
    cw:8,
    bw:8,
    aw:8,
    dz:8,
    cz:8,
    bz:8,
    az:8,
    dy:8,
    cy:8,
    by:8,
    ay:8,
    dx:8,
    cx:8,
    bx:8,
    ax:8,
  >>)
}

fn op_export(x: Int, y: Int, z: Int, w: Int) {
  let #(ax, bx, cx, dx) = bytes(x)
  let #(ay, by, cy, dy) = bytes(y)
  let #(az, bz, cz, dz) = bytes(z)
  let #(aw, bw, cw, dw) = bytes(w)
  from_bit_array(<<
    42:8,
    dw:8,
    cw:8,
    bw:8,
    aw:8,
    dz:8,
    cz:8,
    bz:8,
    az:8,
    dy:8,
    cy:8,
    by:8,
    ay:8,
    dx:8,
    cx:8,
    bx:8,
    ax:8,
  >>)
}

fn bytes(n: Int) -> #(Int, Int, Int, Int) {
  #(
    int.bitwise_shift_right(n, 24),
    int.bitwise_and(int.bitwise_shift_right(n, 16), 0xFF),
    int.bitwise_and(int.bitwise_shift_right(n, 8), 0xFF),
    int.bitwise_and(n, 0xFF),
  )
}

/// little endian, assuming a, b, c, d are in the range [0, 255]
fn unbytes(a: Int, b: Int, c: Int, d: Int) -> Int {
  int.bitwise_or(
    int.bitwise_or(
      int.bitwise_or(
        int.bitwise_shift_left(d, 24),
        int.bitwise_shift_left(c, 16),
      ),
      int.bitwise_shift_left(b, 8),
    ),
    a,
  )
}

fn repeat(n: Int, a: a) -> List(a) {
  case n {
    0 -> []
    1 -> [a]
    _ -> [a, ..repeat(n - 1, a)]
  }
}

fn pad(l: List(Int), n: Int) -> List(Int) {
  case l {
    [] -> repeat(n, 0)
    [h, ..t] -> [h, ..pad(t, n - 1)]
  }
}

fn str_to_id(s: String) -> #(Int, Int, Int, Int) {
  let codepoints =
    string.to_utf_codepoints(s)
    |> list.map(string.utf_codepoint_to_int)
  let assert [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, ..] = pad(codepoints, 16)
  #(
    unbytes(a, b, c, d),
    unbytes(e, f, g, h),
    unbytes(i, j, k, l),
    unbytes(m, n, o, p),
  )
}
