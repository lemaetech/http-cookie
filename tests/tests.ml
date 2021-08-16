let () = Printexc.record_backtrace false

let print_date_time r =
  Fmt.result ~ok:Http_cookie.pp_date_time ~error:Fmt.string Fmt.stdout r

let%expect_test "date_time: year 0 " =
  Http_cookie.date_time ~year:0 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid year (>1600 && < 9999): 0 |}]

let%expect_test "date_time: year 10000" =
  Http_cookie.date_time ~year:10_000 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid year (>1600 && < 9999): 10000 |}]

let%expect_test "date_time: year 2021" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 23 Jan 2021 22:45:59 GMT |}]

let%expect_test "date_time: year 1601" =
  Http_cookie.date_time ~year:1601 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 23 Jan 1601 22:45:59 GMT |}]

let%expect_test "date_time: year 1600" =
  Http_cookie.date_time ~year:1600 ~month:`Jan ~weekday:`Sun ~day:23 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid year (>1600 && < 9999): 1600 |}]

let%expect_test "date_time: day 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:0 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid day of month ( > 0 && <= 31): 0 |}]

let%expect_test "date_time: day 31" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:31 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 31 Jan 2021 22:45:59 GMT |}]

let%expect_test "date_time: day 32" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:32 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid day of month ( > 0 && <= 31): 32 |}]

(* hour tests *)
let%expect_test "date_time: hour 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:0
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid hour (>0 && <24): 0 |}]

let%expect_test "date_time: hour 24" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:24
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid hour (>0 && <24): 24 |}]

let%expect_test "date_time: hour 22" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:45 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:45:59 GMT |}]

(* minute tests *)
let%expect_test "date_time: minute 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:0 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:00:59 GMT |}]

let%expect_test "date_time:minute 24" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:59 GMT |}]

let%expect_test "date_time: minute 60" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:60 ~seconds:59
  |> print_date_time ;
  [%expect {| Invalid minutes (>=0 && < 60): 60 |}]

(* seconds test *)
let%expect_test "date_time: seconds 0" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:0
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:00 GMT |}]

let%expect_test "date_time: seconds 59" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:59
  |> print_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:59 GMT |}]

let%expect_test "date_time: seconds 60" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:60
  |> print_date_time ;
  [%expect {| Invalid seconds (>=0 && < 60): 60 |}]

let%expect_test "date_time: seconds -1" =
  Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day:30 ~hour:22
    ~minutes:24 ~seconds:(-1)
  |> print_date_time ;
  [%expect {| Invalid seconds (>=0 && < 60): -1 |}]

(* create tests *)
let pp_t t =
  Fmt.result ~ok:Http_cookie.pp
    ~error:(fun fmt s -> Fmt.pf fmt "Error: %s" s)
    Fmt.stdout t

(* domain attribute parsing/validation tests *)
let%expect_test "create:  domain= eeee::192.168.0.1" =
  Http_cookie.create ~path:"/hello" ~domain:"eeee::192.168.0.1" ~name:"hello"
    "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: eeee::192.168.0.1
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=10.105.128.1" =
  Http_cookie.create ~path:"/hello" ~domain:"10.105.128.1" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 10.105.128.1
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=152.186.220.254" =
  Http_cookie.create ~path:"/hello" ~domain:"152.186.220.254" ~name:"hello"
    "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 152.186.220.254
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=::" =
  Http_cookie.create ~path:"/hello" ~domain:"::" ~name:"hello" "world" |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: ::
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=::1" =
  Http_cookie.create ~path:"/hello" ~domain:"::1" ~name:"hello" "world" |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: ::1
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=2002:2c26:f4e4:0:21c:42ff:fe20:4636" =
  Http_cookie.create ~path:"/hello"
    ~domain:"2002:2c26:f4e4:0:21c:42ff:fe20:4636" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 2002:2c26:f4e4:0:21c:42ff:fe20:4636
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=fec0::1" =
  Http_cookie.create ~path:"/hello" ~domain:"fec0::1" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: fec0::1
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=fe80::215:5dff:fe00:402" =
  Http_cookie.create ~path:"/hello" ~domain:"fe80::215:5dff:fe00:402"
    ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: fe80::215:5dff:fe00:402
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=2002:2c26:f4e4:0:21c:42ff:fe20:4636" =
  Http_cookie.create ~path:"/hello"
    ~domain:"2002:2c26:f4e4:0:21c:42ff:fe20:4636" ~name:"hello" "world"
  |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path: /hello
    domain: 2002:2c26:f4e4:0:21c:42ff:fe20:4636
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create:  domain=23ah" =
  Http_cookie.create ~path:"/hello" ~domain:"23ah" ~name:"hello" "world" |> pp_t ;
  [%expect {|
    Error: domain: 23ah |}]

let%expect_test "create:  domain=2333::ddd::1" =
  Http_cookie.create ~path:"/hello" ~domain:"2333::ddd::1" ~name:"hello" "world"
  |> pp_t ;
  [%expect {|
    Error: domain: 2333::ddd::1 |}]

(* name tests *)
let%expect_test "create: name=he@llo" =
  Http_cookie.create ~name:"he@llo" "world" |> pp_t ;
  [%expect {| Error: name: he@llo |}]

let%expect_test "create: name=he(llo" =
  Http_cookie.create ~name:"he(llo" "world" |> pp_t ;
  [%expect {| Error: name: he(llo |}]

let%expect_test "create: name=he>llo" =
  Http_cookie.create ~name:"he>llo" "world" |> pp_t ;
  [%expect {| Error: name: he>llo |}]

let%expect_test "create: name=he<llo" =
  Http_cookie.create ~name:"he<llo" "world" |> pp_t ;
  [%expect {| Error: name: he<llo |}]

(* value tests *)
let%expect_test "create: value=val dd (space ' ' is invalid)" =
  Http_cookie.create ~name:"hello" "val dd" |> pp_t ;
  [%expect {| Error: value: val dd |}]

let%expect_test "create: value=val,dd (',' is invalid)" =
  Http_cookie.create ~name:"hello" "val,dd" |> pp_t ;
  [%expect {| Error: value: val,dd |}]

(* path tests *)
let%expect_test "create: path=val;dd (';' is invalid)" =
  Http_cookie.create ~path:"val;dd" ~name:"hello" "value" |> pp_t ;
  [%expect {| Error: path: val;dd |}]
