(*-------------------------------------------------------------------------
 * Copyright (c) 2019, 2020 Bikal Gurung. All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License,  v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * %%NAME%% %%VERSION%%
 *-------------------------------------------------------------------------*)

exception Cookie of string

type t =
  { name: string
  ; value: string
  ; path: string option
  ; domain: string option
  ; expires: date_time option
  ; max_age: int option
  ; secure: bool option
  ; http_only: bool option
  ; same_site: same_site option
  ; extension: string option }

and date_time =
  { year: int
  ; month:
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
  ; weekday: [`Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat]
  ; day_of_month: int
  ; hour: int
  ; minutes: int
  ; seconds: int }

and same_site = [`None | `Lax | `Strict]

let compare (t1 : t) (t2 : t) = compare t1 t2
let name c = c.name
let value c = c.value
let path c = c.path
let domain c = c.domain
let expires c = c.expires
let max_age c = c.max_age
let extension c = c.extension
let same_site c = c.same_site
let http_only c = c.http_only
let secure c = c.secure

let rec pp fmt' t =
  let fields =
    [ Fmt.field "name" (fun p -> p.name) Fmt.string
    ; Fmt.field "value" (fun p -> p.value) Fmt.string
    ; Fmt.field "path" (fun p -> p.path) Fmt.(option string)
    ; Fmt.field "domain" (fun p -> p.domain) Fmt.(option string)
    ; Fmt.field "expires" (fun p -> p.expires) Fmt.(option pp_date_time)
    ; Fmt.field "max_age" (fun p -> p.max_age) Fmt.(option int)
    ; Fmt.field "secure" (fun p -> p.secure) Fmt.(option bool)
    ; Fmt.field "http_only" (fun p -> p.http_only) Fmt.(option bool)
    ; Fmt.field "same_site" (fun p -> p.same_site) Fmt.(option pp_same_site)
    ; Fmt.field "extension" (fun p -> p.extension) Fmt.(option string) ]
  in
  Fmt.record fields fmt' t

and pp_date_time fmt tm =
  let weekday =
    match tm.weekday with
    | `Sun -> "Sun"
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
  in
  let month =
    match tm.month with
    | `Jan -> "Jan"
    | `Feb -> "Feb"
    | `Mar -> "Mar"
    | `Apr -> "Apr"
    | `May -> "May"
    | `Jun -> "Jun"
    | `Jul -> "Jul"
    | `Aug -> "Aug"
    | `Sep -> "Sep"
    | `Oct -> "Oct"
    | `Nov -> "Nov"
    | `Dec -> "Dec"
  in
  Format.fprintf fmt "%s, %02d %s %04d %02d:%02d:%02d GMT" weekday
    tm.day_of_month month tm.year tm.hour tm.minutes tm.seconds

and pp_same_site fmt = function
  | `None -> Format.fprintf fmt "None"
  | `Lax -> Format.fprintf fmt "Lax"
  | `Strict -> Format.fprintf fmt "Strict"

and to_string pp t =
  let buf = Buffer.create 0 in
  let fmt = Format.formatter_of_buffer buf in
  Format.fprintf fmt "%a%!" pp t ;
  Buffer.contents buf

let date_to_string tm = to_string pp_date_time tm
let same_site_to_string ss = to_string pp_same_site ss

(* Date time *)
let ( let* ) r f = Result.bind r f

let compare_date_time (dt1 : date_time) (dt2 : date_time) =
  Stdlib.compare dt1 dt2

let date_time ~year ~month ~weekday ~day_of_month ~hour ~minutes ~seconds =
  let* year =
    if year > 0 && year < 9999 then Ok year
    else Error (Format.sprintf "Invalid year (>0 && < 9999): %d" year)
  in
  let* day_of_month =
    if day_of_month > 0 && day_of_month <= 31 then Ok day_of_month
    else
      Error
        (Format.sprintf "Invalid day of month ( > 0 && <= 31): %d" day_of_month)
  in
  let* hour =
    if hour > 0 && hour < 24 then Ok hour
    else Error (Format.sprintf "Invalid hour (>0 && <24): %d" hour)
  in
  let* minutes =
    if minutes >= 0 && minutes < 60 then Ok minutes
    else Error (Format.sprintf "Invalid minutes (>=0 && < 60): %d" minutes)
  in
  let* seconds =
    if seconds >= 0 && seconds < 60 then Ok seconds
    else Error (Format.sprintf "Invalid seconds (>=0 && < 60): %d" seconds)
  in
  Ok {year; month; weekday; day_of_month; hour; minutes; seconds}

let is_control_char c =
  let code = Char.code c in
  (code >= 0 && code <= 31) || code = 127

let err fmt = Format.ksprintf (fun s -> raise (Cookie s)) fmt

(* Parses cookie attribute value. Cookie attribute values shouldn't contain any
   CTL(control characters) or ';' char. *)
let parse_cookie_av attr_value err =
  let rec validate i av =
    if i >= String.length av then av
    else
      let c = av.[i] in
      if is_control_char c || Char.equal c ';' then err c
      else validate (i + 1) av
  in
  match attr_value with
  | None -> None
  | Some value when String.length value = 0 -> None
  | Some value -> Some (validate 0 value)

let parse_path path =
  err "Cookie 'Path' attribute value contains invalid character '%c'"
  |> parse_cookie_av path

let parse_extension extension =
  err "Cookie extension value contains invalid character '%c'"
  |> parse_cookie_av extension

(* Parses a given cookie into a cookie_name. Valid cookie name/token as defined
   in https://tools.ietf.org/html/rfc2616#section-2.2 *)
let parse_name name =
  let is_separator = function
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
     |']' | '?' | '=' | '{' | '}' | ' ' ->
        true
    | c when Char.code c = 9 -> true
    | _ -> false
  in
  let is_us_ascii_char c =
    let code = Char.code c in
    code >= 0 && code <= 127
  in
  let rec validate i name =
    if i >= String.length name then name
    else
      let c = name.[i] in
      match c with
      | c when is_control_char c ->
          err "Control character '%c' found in name." c
      | c when is_separator c -> err "Separator character '%c' found in name." c
      | c when not (is_us_ascii_char c) ->
          err "Invalid US-ASCII character '%c' found in name." c
      | _ -> validate (i + 1) name
  in
  let name =
    if String.length name > 0 then name else err "0 length cookie name."
  in
  validate 0 name

(* Based https://tools.ietf.org/html/rfc6265#section-4.1.1
 * cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
 * cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
 *
 * US-ASCII characters excluding CTLs, whitespace DQUOTE, comma, semicolon,
 * and backslash
 *
 * We loosen this as spaces and commas are common in cookie values but we produce
 * a quoted cookie-value in when value starts or ends with a comma or space.
 * See https://golang.org/issue/7243 for the discussion.
 *)
let parse_value value =
  let dquote = Char.code '"' in
  let semi = Char.code ';' in
  let b_slash = Char.code '\\' in
  let rec validate i s =
    if i >= String.length s then s
    else
      let c = s.[i] in
      let code = Char.code c in
      if
        0x20 <= code && code < 0x7f && code <> dquote && code <> semi
        && code <> b_slash
      then validate (i + 1) s
      else err "Invalid char '%c' found in cookie value" c
  in
  let strip_quotes s =
    let is_dquote = String.equal "\"" in
    let first_s = String.sub s 0 1 in
    let last_s = String.sub s (String.length s - 1) 1 in
    if is_dquote first_s && is_dquote last_s then
      String.sub s 1 (String.length s - 2)
    else s
  in
  let value =
    if String.length value > 0 then value
    else err "Cookie value length must be > 0."
  in
  strip_quotes value |> validate 0

(** See https://tools.ietf.org/html/rfc1034#section-3.5 and
    https://tools.ietf.org/html/rfc1123#section-2 *)
let parse_domain_av domain_av =
  let rec validate last_char label_count (i, s) =
    if i >= String.length s then (s, last_char, label_count)
    else
      let label_count = label_count + 1 in
      let c = s.[i] in
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> validate c label_count (i + 1, s)
      | '-' ->
          if Char.equal last_char '.' then
            err "Character before '-' cannot be '.'"
          else validate c label_count (i + 1, s)
      | '.' ->
          if Char.equal last_char '.' || Char.equal last_char '-' then
            err "Character before '.' cannot be '.' or '-'"
          else if label_count > 63 || label_count = 0 then
            err "Domain name label can't exceed 63 characters or have 0 length"
          else validate c 0 (i + 1, s) (* reset label_count *)
      | _ -> err "Invalid character '%c'" c
  in
  let validate_length av =
    if String.length av > 255 then
      err "Domain attribute value length must not exceed 255 characters"
    else ()
  in
  let validate_last_char last_char =
    if Char.equal '-' last_char then
      err "Domain attribute value's last character is not allowed to be '-'"
    else ()
  in
  let validate_label_length label_count =
    if label_count > 63 then
      err "Domain attribute value label length can't exceed 63 characters"
    else ()
  in
  match domain_av with
  | None -> None
  | Some domain_av when String.length domain_av = 0 -> None
  | Some domain_av ->
      let () = validate_length domain_av in
      let domain_av =
        if String.equal "." (String.sub domain_av 0 1) then
          (* A cookie domain attribute may start with a leading dot. *)
          String.sub domain_av 0 1
        else domain_av
      in
      let domain_av, last_char, label_count = validate '.' 0 (0, domain_av) in
      let domain_av = domain_av in
      let () = validate_last_char last_char in
      let () = validate_label_length label_count in
      Some domain_av

let parse_max_age max_age =
  match max_age with
  | None -> None
  | Some ma ->
      if ma <= 0 then
        err "Cookies 'Max-Age' attribute is less than or equal to 0"
      else Option.some ma

let create ?path ?domain ?expires ?max_age ?secure ?http_only ?same_site
    ?extension name ~value =
  let name = parse_name name in
  let value = parse_value value in
  let domain = parse_domain_av domain in
  let path = parse_path path in
  let max_age = parse_max_age max_age in
  let extension = parse_extension extension in
  { name
  ; value
  ; path
  ; domain
  ; expires
  ; max_age
  ; secure
  ; http_only
  ; same_site
  ; extension }

(* https://datatracker.ietf.org/doc/html/rfc6265#section-4.2.1

   cookie-header = "Cookie:" OWS cookie-string OWS
   cookie-string = cookie-pair *( ";" SP cookie-pair )
*)
let of_cookie header =
  let open Angstrom in
  let token =
    take_while1 (function
      | '\x00' .. '\x1F' | '\x7F' -> false (* CONTROL chars *)
      | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
       |']' | '?' | '=' | '{' | '}' | ' ' ->
          false (* SEPARATOR chars *)
      | _ -> true )
  in
  let cookie_name = token in
  let cookie_octet = function
    | '\x21'
     |'\x23' .. '\x2B'
     |'\x2D' .. '\x3A'
     |'\x3C' .. '\x5B'
     |'\x5D' .. '\x7E' ->
        true
    | _ -> false
  in
  let cookie_value =
    take_while cookie_octet <|> (char '"' *> take_while cookie_octet <* char '"')
  in
  let cookie_pair =
    lift3
      (fun cookie_name' _ cookie_value' -> (cookie_name', cookie_value'))
      cookie_name (char '=') cookie_value
  in
  let cookie_string = sep_by1 (char ';' *> char '\x20') cookie_pair in
  let ows = skip_while (function '\x20' | '\t' -> true | _ -> false) in
  let cookies = ows *> cookie_string <* ows in
  parse_string ~consume:All cookies header
  |> Result.map (fun cookies' ->
         List.map
           (fun (cookie_name', cookie_value') ->
             create cookie_name' ~value:cookie_value' )
           cookies' )

let to_set_cookie t =
  let module O = Option in
  let buf = Buffer.create 50 in
  let add_str fmt = Format.ksprintf (Buffer.add_string buf) fmt in
  add_str "%s=%s" (name t) (value t) ;
  O.iter (fun path -> add_str "; Path=%s" path) (path t) ;
  O.iter (fun d -> add_str "; Domain=%s" d) (domain t) ;
  O.iter
    (fun expires -> add_str "; Expires=%s" @@ date_to_string expires)
    (expires t) ;
  O.iter
    (fun max_age -> if max_age > 0 then add_str "; Max-Age=%d" max_age)
    (max_age t) ;
  O.iter (fun secure -> if secure then add_str "; Secure") t.secure ;
  O.iter (fun http_only -> if http_only then add_str "; HttpOnly") t.http_only ;
  O.iter
    (fun same_site -> add_str "; SameSite=%s" (same_site_to_string same_site))
    t.same_site ;
  O.iter (fun extension -> add_str "; %s" extension) (extension t) ;
  Buffer.contents buf

let to_cookie t = Format.sprintf "%s=%s" (name t) (value t)

(* Updates. *)
let update_value v c = {c with value= parse_value v}
let update_name name c = {c with name= parse_name name}
let update_path p c = {c with path= parse_path p}
let update_domain d c = {c with domain= parse_domain_av d}
let update_expires expires c = {c with expires}
let update_max_age m c = {c with max_age= parse_max_age m}
let update_secure secure c = {c with secure}
let update_http_only http_only c = {c with http_only}
let update_same_site same_site c = {c with same_site}
let update_extension extension c = {c with extension}
