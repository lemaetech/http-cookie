(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

type t
(** Represents 'Same-site' cookie attribute. See
    https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00. *)

val default : t
val none : t
val lax : t
val strict : t
val equal : t -> t -> bool
val compare : t -> t -> int
val to_string : t -> string
