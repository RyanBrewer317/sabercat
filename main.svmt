/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/* allocate a region, call `user` with the continuation `destroyer` */
fn main: ()->0 = 
    new_rgn =r $destroyer <r> app $user call;

/*
 * put a 1-element tuple in the given region, put 3 in the tuple, 
 * get 3 out, print 3, call continuation 
 */
fn user: Forall r: Rgn. (handle(r), (handle(r))->0)->0 =
    1 get <(i32)@r> malloc 3 0 init 0 proj print call;

/* free the given region and halt the program normally */
fn destroyer: Forall r: Rgn!. (handle(r))->0 =
    free_rgn 0 halt;
