/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

fn main: ()->0 = 
    200 new_rgn =r $destroyer <r> app $user call;

fn user: Forall r: Rgn. (handle(r), (handle(r))->0)->0 =
    1 get 1 <i32[]@r> malloc 4 0 arr_init 0 arr_proj print call;

fn destroyer: Forall r: Rgn!. (handle(r))->0 =
    free_rgn 0 halt;
