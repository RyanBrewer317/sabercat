/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/* an array of bytes at the start of the executable */
data: [2 0 0 0 3 0 0 0 4 0 0 0]

/* 
 * `<i32[]@data_section> 4 data` pushes an immutable pointer to the 4th byte of the data section
 * interpreting the bytes from there to the end of the data section as an i32 array
 */
fn main: ()->0 = <i32[]@data_section> 4 data 0 arr_proj print 0 halt;



