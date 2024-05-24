// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import assembler
import parser
import party
import shellout.{arguments}
import simplifile

pub fn main() {
  let filename = case arguments() {
    [] -> "main.sc"
    [s, ..] -> s
  }
  let assert Ok(s) = simplifile.read(filename)
  let assert Ok(#(data_section, imports, parsed)) = party.go(parser.go(), s)
  let assert Ok(builder) = assembler.go(data_section, imports, parsed)
  let assert Ok(_) = simplifile.write_bits(builder, to: "bin.svm")
  let assert Ok(_) = shellout.command(
    in: ".",
    run: "../SaberVM/target/release/sabervm",
    with: ["bin.svm"],
    opt: [shellout.LetBeStderr, shellout.LetBeStdout],
  )
}
