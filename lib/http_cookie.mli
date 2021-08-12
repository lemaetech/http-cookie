(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

(** A comprehensive and standards compliant HTTP cookies library for ocaml.

    HTTP cookie is serialized as follows:

    - In a [Cookie] header in a HTTP request
    - In a [Set-Cookie] header in a HTTP response.

    The library supports consuming and creating HTTP cookie in both requests and
    responses.

    The standard implemented by the library is
    {{:https://tools.ietf.org/html/rfc6265} RFC 6265}. *)

(** A HTTP cookie. *)
type t

(** Cookie expiry datetime value. *)
and date_time

(** 'Same-site' cookie attribute. See
    https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00. *)
and same_site = [`None | `Lax | `Strict]

(** raised when one of the cookie attribute values are invalid. *)
exception Cookie of string

(** {1 Http_cookie} *)

val date_time :
     year:int
  -> month:
       [ `Jan
       | `Feb
       | `Mar
       | `Apr
       | `May
       | `Jun
       | `Jul
       | `Aug
       | `Sep
       | `Oct
       | `Nov
       | `Dec ]
  -> weekday:[`Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat]
  -> day_of_month:int
  -> hour:int (** 24 hour format *)
  -> minutes:int
  -> seconds:int
  -> (date_time, string) result
(** [date_time] is [Ok dt] if all of the given parameters are valid for creating
    {!type:date_time} value, otherwise [Error err] is denotes the error. *)

val create :
     ?path:string
  -> ?domain:string
  -> ?expires:date_time
  -> ?max_age:int
  -> ?secure:bool
  -> ?http_only:bool
  -> ?same_site:same_site
  -> ?extension:string
  -> string
  -> value:string
  -> (t, string) result
(** [create ~path ~domain ~expires ~max_age ~secure ~http_only ~same_site ~extension name
    ~value]
    returns a cookie instance {!type:t} with cookie name [name] and value
    [value] along with the given attributes.

    @raise Cookie if any of the cookie attributes fail validation. *)

val of_cookie : string -> (t list, string) result
(** [of_cookie header] parses [header] - a string value which represents HTTP
    [Cookie] header value as defined in
    {:https://tools.ietf.org/html/rfc6265#section-4.2}. It returns a list of
    [Cookie]s if it is able to successfully parse [s], otherwise it returns
    [Error err].

    {4 Examples}

    This returns two cookies with cookie names [SID] and [lang-en].

    {[ Http_cookie.of_cookie "SID=31d4d96e407aad42; lang=en-US" ]} *)

val to_set_cookie : t -> string
(** [to_set_cookie c] serializes cookie [c] into a string which can be encoded
    as value for HTTP [Set-Cookie] header.

    The datetime format for [expires] attribute is specified in
    {{:https://tools.ietf.org/html/rfc2616#section-3.3.1} RFC 2616}

    Example of a string returned by the function,

    {v
SID=31d4d96e407aad42; Path=/; Secure; HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT
    v} *)

val to_cookie : t -> string
(** [to_cookie c] serializes [c] into a string which can be encoded as value for
    HTTP [Cookie] header.

    Example of a string returned by the function.

    {v SID=31d4d96e407aad42 v} *)

(** {1 Cookie Attributes}

    Cookie attributes are defined precisely at
    {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} RFC 6262} *)

val name : t -> string
(** [name t] returns a cookie name.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} cookie-name} *)

val value : t -> string
(** [value t] returns a cookie value.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} cookie-value} *)

val path : t -> string option
(** [path t] returns cookie path attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-5.2.4} cookie-path} *)

val domain : t -> string option
(** [domain t] returns cookie domain attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.3} cookie-domain} *)

val expires : t -> date_time option
(** [expires t] returns a coookie expires attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.1.} cookie-expires} *)

val max_age : t -> int option
(** [max_age t] returns a cookie max_age attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.2} max-age} and
    {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} max-age-av} *)

val secure : t -> bool option
(** [secure t] returns a secure attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.5} cookie-secure} *)

val http_only : t -> bool option
(** [http_only t] returns a http_only attribute.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.2.6} http-only} *)

val same_site : t -> same_site option
(** [same_site t] returns a same_site attribute.

    See {{:https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00}
    same-site} *)

val extension : t -> string option
(** [extension t] returns a cookie extension value.

    See {{:https://tools.ietf.org/html/rfc6265#section-4.1.1} cookie-extension} *)

(** {1 Compare} *)

val compare : t -> t -> int
(** [compare c1 c2] returns [0] if [c1] and [c2] are equal, a positive integer
    if [c1] is greater than [c2] and a negative integer if [c1] is less than
    [c2] *)

val compare_date_time : date_time -> date_time -> int

(** {1 Updates}

    All of the update functions may raise [Cookie err] exception if attempt is
    made to update a cookie with invalid values. *)

val update_value : string -> t -> t
val update_name : string -> t -> t
val update_path : string option -> t -> t
val update_domain : string option -> t -> t
val update_expires : date_time option -> t -> t
val update_max_age : int option -> t -> t
val update_secure : bool option -> t -> t
val update_http_only : bool option -> t -> t
val update_same_site : same_site option -> t -> t
val update_extension : string option -> t -> t

(** {1 Pretty Printers} *)

val pp : Format.formatter -> t -> unit
val pp_date_time : Format.formatter -> date_time -> unit
val pp_same_site : Format.formatter -> same_site -> unit
