(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** The [Cookie] module implements HTTP Cookies sent in a 'Cookie' header in a
    http request or 'Set-Cookie' headers added in a http response.

    The module closely follows the HTTP Cookie specification as defined in
    RFC6265 [https://tools.ietf.org/html/rfc6265]. *)

type t
(** Represents either a Cookie name-value in 'Cookie' request header or a set of
    cookie attributes in 'Set-Cookie' response header. *)

exception Cookie of string

val create :
     name:string
  -> value:string
  -> ?sanitize_name:bool
  -> ?sanitize_value:bool
  -> ?path:string
  -> ?domain:string
  -> ?expires:Unix.tm
  -> ?max_age:int
  -> ?secure:bool
  -> ?http_only:bool
  -> ?same_site:Same_site.t
  -> ?extension:string
  -> unit
  -> t
(** [create ... ()] parse a cookie instance [t] from given value parameters. If
    the given values parses successfully, then [Ok t] is returned. If an error
    is encountered while parsing then an [Error err] is returned.

    [~sanitize_value] if true then [value] is double quoted if it starts or ends
    in a ' '(space) or a ','(comma) character.

    [~sanitize_name] if true and [name] contains '\n' or '\r' character then it
    is replaced by '-' character.

    @raise Cookie If any of the arguments fail validation. *)

val compare : t -> t -> int

val name : t -> string
(** [name t] returns a cookie name. See
    https://tools.ietf.org/html/rfc6265#section-4.1.1 'cookie-name' definition
    for details. *)

val value : t -> string
(** [value t] returns a cookie value. See 'cookie-value' definition in
    https://tools.ietf.org/html/rfc6265#section-4.1.1 *)

val path : t -> string option
(** [path t] returns cookie path attribute. See
    https://tools.ietf.org/html/rfc6265#section-5.2.4 *)

val domain : t -> string option
(** [domain t] returns cookie domain attribute. See
    https://tools.ietf.org/html/rfc6265#section-4.1.2.3 *)

val expires : t -> Unix.tm option
(** [expires t] returns a coookie expires attribute. See
    https://tools.ietf.org/html/rfc6265#section-4.1.2.1.

    Additionally, the date format followed is as specified in
    https://tools.ietf.org/html/rfc2616#section-3.3.1*)

val max_age : t -> int option
(** [max_age t] returns a cookie max_age attribute. See
    https://tools.ietf.org/html/rfc6265#section-4.1.2.2.

    Additionally, see 'max-age-av' in
    https://tools.ietf.org/html/rfc6265#section-4.1.1 *)

val secure : t -> bool option
(** [secure t] returns a secure attribute. See
    https://tools.ietf.org/html/rfc6265#section-4.1.2.5 *)

val http_only : t -> bool option
(** [http_only t] returns a http_only attribute. See
    https://tools.ietf.org/html/rfc6265#section-4.1.2.6 *)

val same_site : t -> Same_site.t option
(** [same_site t] returns a same_site attribute. See
    https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00. *)

val extension : t -> string option
(** [extension t] returns a cookie extension value. See k
    https://tools.ietf.org/html/rfc6265#section-4.1.1 *)

val of_cookie_header : string -> t list
(** [of_cookie_header s] parses [s] - a string value which represents HTTP
    'Cookie' header value as defined in
    https://tools.ietf.org/html/rfc6265#section-4.2 and returns a list of
    [Cookie]s. Examples of [s] is [SID=31d4d96e407aad42; lang=en-US]. Here we
    define 2 cookies 'SID' and 'lang-en'. *)

val to_set_cookie_header_value : t -> string
(** [to_set_header c] serializes [c] into a string which can be used as value
    for HTTP 'Set-Cookie' header. *)

val to_cookie_header_value : t -> string
(** [to_cookie_header c] serializes [c] into a string which can used as value
    for HTTP 'Cookie' header. *)
