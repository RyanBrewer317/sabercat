/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

data: [82 121 97 110 32 108 111 118 101 115 32 73 118 121 33 10]

import print_and_halt: (u8[]@data_section)->0;

fn main: ()->0 = 
    <u8[]@data_section> 0 data 
    print
    100 new_rgn =r 
    <u8[]@r> 5 malloc 
    10u8 4 arr_mut 
    <u8[]@data_section> 0 data 
    4 copy_n 
    $print_and_halt call;


