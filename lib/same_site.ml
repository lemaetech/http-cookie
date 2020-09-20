(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

type t =
  [ `Default
  | `None
  | `Lax
  | `Strict ]

let default = `Default
let none = `None
let lax = `Lax
let strict = `Strict
let compare (t1 : t) (t2 : t) = compare t1 t2
let equal (t1 : t) (t2 : t) = compare t1 t2 = 0

let to_string = function
  | `Default -> ""
  | `None    -> "None"
  | `Lax     -> "Lax"
  | `Strict  -> "Strict"
