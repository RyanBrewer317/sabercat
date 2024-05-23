// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub type Stmt {
  Stmt(name: String, t: Type, body: Expr)
}

pub type Expr {
  I32Lit(Int)
  U8Lit(Int)
  Func(String)
  Instr(String)
  Type(Type)
  CTAssignment(String)
  Compose(Expr, Expr)
}

pub type Type {
  I32
  U8
  TVar(String)
  FuncType(List(Type))
  TupleType(List(Type))
  Ptr(Type, String)
  Forall(String, Int, Type)
  ForallRgn(String, Bool, Type)
  Exists(String, Int, Type)
  Handle(String)
  Array(Type, String)
}
