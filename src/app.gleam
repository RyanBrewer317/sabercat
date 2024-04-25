// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import simplifile
import party
import parser
import assembler
import shellout.{arguments}

pub fn main() {
  let filename = case arguments() {
    [] -> "main.sc"
    [s, ..] -> s
  }
  let assert Ok(s) = simplifile.read(filename)
  let assert Ok(parsed) = party.go(parser.go(), s)
  let assert Ok(builder) = assembler.go(parsed)
  simplifile.write_bits(builder, to: "bin.svm")
}
