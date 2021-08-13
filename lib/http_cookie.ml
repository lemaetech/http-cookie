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

(* Pretty Printers *)
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

(* Compare *)
let compare (t1 : t) (t2 : t) = Stdlib.compare t1 t2

let compare_date_time (dt1 : date_time) (dt2 : date_time) =
  Stdlib.compare dt1 dt2

(* Cookie parsers.

   https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.1

   set-cookie-header = "Set-Cookie:" SP set-cookie-string
   set-cookie-string = cookie-pair *( ";" SP cookie-av )
   cookie-pair       = cookie-name "=" cookie-value
   cookie-name       = token
   cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
   cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                         ; US-ASCII characters excluding CTLs,
                         ; whitespace DQUOTE, comma, semicolon,
                         ; and backslash
   token             = <token, defined in [RFC2616], Section 2.2>

   cookie-av         = expires-av / max-age-av / domain-av /
                       path-av / secure-av / httponly-av /
                       extension-av
   expires-av        = "Expires=" sane-cookie-date
   sane-cookie-date  = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
   max-age-av        = "Max-Age=" non-zero-digit *DIGIT
                         ; In practice, both expires-av and max-age-av
                         ; are limited to dates representable by the
                         ; user agent.
   non-zero-digit    = %x31-39
                         ; digits 1 through 9
   domain-av         = "Domain=" domain-value
   domain-value      = <subdomain>
                         ; defined in [RFC1034], Section 3.5, as
                         ; enhanced by [RFC1123], Section 2.1
   path-av           = "Path=" path-value
   path-value        = <any CHAR except CTLs or ";">
   secure-av         = "Secure"
   httponly-av       = "HttpOnly"
   extension-av      = <any CHAR except CTLs or ";">
*)
open Angstrom

let token =
  take_while1 (function
    | '\x00' .. '\x1F' | '\x7F' -> false (* CONTROL chars *)
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
     |']' | '?' | '=' | '{' | '}' | ' ' ->
        false (* SEPARATOR chars *)
    | _ -> true )

let cookie_name = token

let cookie_value =
  let cookie_octet = function
    | '\x21'
     |'\x23' .. '\x2B'
     |'\x2D' .. '\x3A'
     |'\x3C' .. '\x5B'
     |'\x5D' .. '\x7E' ->
        true
    | _ -> false
  in
  take_while cookie_octet <|> (char '"' *> take_while cookie_octet <* char '"')

let cookie_pair =
  let* name = cookie_name in
  let+ value = char '=' *> cookie_value in
  (name, value)

let _ows = skip_while (function '\x20' | '\t' -> true | _ -> false)

(* https://datatracker.ietf.org/doc/html/rfc6265#section-4.2.1

   cookie-header = "Cookie:" OWS cookie-string OWS
   cookie-string = cookie-pair *( ";" SP cookie-pair )
*)
let cookie_string = sep_by1 (char ';' *> char '\x20') cookie_pair

(* Domain attribute value:

    domain-value      = <subdomain>
                       ; defined in [RFC1034], Section 3.5, as
                       ; enhanced by [RFC1123], Section 2.1

   https://datatracker.ietf.org/doc/html/rfc1034#section-3.5

    <subdomain> ::= <label> | <subdomain> "." <label>
    <label> ::= <letter> [ [ <ldh-str> ] <let-dig> ]
    <ldh-str> ::= <let-dig-hyp> | <let-dig-hyp> <ldh-str>
    <let-dig-hyp> ::= <let-dig> | "-"
    <let-dig> ::= <letter> | <digit>
    <letter> ::= any one of the 52 alphabetic characters A through Z or a to z
    <digit> ::= any one of the ten digits 0 through 9
*)
let string_of_list l = List.to_seq l |> String.of_seq

let domain_value =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let digit = satisfy is_digit in
  let letter = satisfy is_letter in
  let let_dig = letter <|> digit in
  let let_dig_hyp = let_dig <|> char '-' in
  let ldh_str = many1 let_dig_hyp in
  let label =
    let* first_char = letter in
    let* middle_chars = option [] ldh_str in
    let+ middle_chars =
      let len = List.length middle_chars in
      if len > 0 && len <= 63 then
        let last_char = List.nth middle_chars (len - 1) in
        if is_digit last_char || is_letter last_char then return middle_chars
        else
          fail
            (Format.sprintf
               "Invalid 'Domain' cookie attribute value: %s. middle characters \
                must end with either a letter or a digit"
               (string_of_list middle_chars) )
      else if len = 0 then return []
      else
        fail
          (Format.sprintf
             "Invalid 'Domain' cookie attribute value: %s. label must 63 \
              characters or less."
             (string_of_list middle_chars) )
    in
    string_of_list (first_char :: middle_chars)
  in
  let subdomain = sep_by1 (char '.') label in
  (* A cookie domain attribute value may start with a leading dot which
     can ignored.

     https://datatracker.ietf.org/doc/html/rfc6265#section-4.1.2.3
  *)
  let* () = option () (char '.' *> return ()) in
  let* start_pos = pos in
  let* subdomain = subdomain in
  let* end_pos = pos in
  let len = end_pos - start_pos in
  if len > 255 then fail "Domain attribute length exceeds 255 characters"
  else return (String.concat "." subdomain)

let cookie_av =
  take_while1 (function
    | '\x00' .. '\x1F' | '\x7F' -> false (* CONTROL chars *)
    | ';' -> false
    | _ -> true )

let path_value = cookie_av
let extension_value = cookie_av
let parse_name name = parse_string ~consume:Consume.All cookie_name name
let parse_value value = parse_string ~consume:Consume.All cookie_value value

let parse_max_age max_age =
  match max_age with
  | None -> Ok None
  | Some ma ->
      if ma <= 0 then
        Error "Cookies 'Max-Age' attribute is less than or equal to 0"
      else Ok (Some ma)

let parse p input =
  match input with
  | Some input -> parse_string ~consume:Consume.All (p >>| Option.some) input
  | None -> Ok None

let ( let* ) r f = Result.bind r f
let ( let+ ) r f = Result.map f r

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

let create ?path ?domain ?expires ?max_age ?secure ?http_only ?same_site
    ?extension ~name value =
  let* name = parse_name name in
  let* value = parse_value value in
  let* domain = parse domain_value domain in
  let* path = parse path_value path in
  let* max_age = parse_max_age max_age in
  let+ extension = parse extension_value extension in
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

let of_cookie header =
  parse_string ~consume:All cookie_string header
  |> Result.map (fun cookies' ->
         List.map
           (fun (name, value) ->
             { name
             ; value
             ; path= None
             ; domain= None
             ; expires= None
             ; max_age= None
             ; secure= None
             ; http_only= None
             ; same_site= None
             ; extension= None } )
           cookies' )

let to_cookie t = Format.sprintf "%s=%s" t.name t.value

let to_set_cookie t =
  let module O = Option in
  let buf = Buffer.create 50 in
  let add_str fmt = Format.ksprintf (Buffer.add_string buf) fmt in
  add_str "%s=%s" t.name t.value ;
  O.iter (fun path -> add_str "; Path=%s" path) t.path ;
  O.iter (fun d -> add_str "; Domain=%s" d) t.domain ;
  O.iter
    (fun expires -> add_str "; Expires=%s" @@ date_to_string expires)
    t.expires ;
  O.iter
    (fun max_age -> if max_age > 0 then add_str "; Max-Age=%d" max_age)
    t.max_age ;
  O.iter (fun secure -> if secure then add_str "; Secure") t.secure ;
  O.iter (fun http_only -> if http_only then add_str "; HttpOnly") t.http_only ;
  O.iter
    (fun same_site -> add_str "; SameSite=%s" (same_site_to_string same_site))
    t.same_site ;
  O.iter (fun extension -> add_str "; %s" extension) t.extension ;
  Buffer.contents buf

let of_set_cookie _s = failwith "not implemented"

(* Attributes *)
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

(* Updates. *)
let update_value value cookie =
  let+ value = parse_value value in
  {cookie with value}

let update_name name cookie =
  let+ name = parse_name name in
  {cookie with name}

let update_path path cookie =
  let+ path = parse path_value path in
  {cookie with path}

let update_domain domain cookie =
  let+ domain = parse domain_value domain in
  {cookie with domain}

let update_expires expires cookie = {cookie with expires}

let update_max_age max_age cookie =
  let+ max_age = parse_max_age max_age in
  {cookie with max_age}

let update_secure secure cookie = {cookie with secure}
let update_http_only http_only cookie = {cookie with http_only}
let update_same_site same_site cookie = {cookie with same_site}

let update_extension extension cookie =
  let+ extension = parse extension_value extension in
  {cookie with extension}
