// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import simplifile
import party
import parser
import assembler
import gleam/list

pub fn main() {
  let assert Ok(s) = simplifile.read("main.svmt")
  let assert Ok(parsed) = party.go(parser.go(), s)
  let funcs = list.index_fold()
  simplifile.write_bits(parsed, to: "bin.svm")
}