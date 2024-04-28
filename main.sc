/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

fn main: ()->0 = 
    7 $end $fact call;

/* in, out, k */
fn fact_helper: (i32, i32, (i32)->0)->0 =
    2 get $fact_step $fact_base callnz;

/* in, out, k */
fn fact_base: (i32, i32, (i32)->0)->0 =
    call;

/* in, out, k */
fn fact_step: (i32, i32, (i32)->0)->0 =
    2 get /*in,out,k,in*/ -1 addi32 /*in-1*/ 3 get 3 get muli32 /*in*out*/ 2 get /*k*/ $fact_helper call;

fn fact: (i32, (i32)->0)->0 =
    1 get 1 2 get $fact_helper call;

fn end: (i32)->0 =
    print 0 halt;
