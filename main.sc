/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

pub fn print_and_halt: Forall r: Rgn. (u8[]@r)->0 =
    print
    0u8 halt;

