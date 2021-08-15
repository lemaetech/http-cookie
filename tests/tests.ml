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

let%expect_test "create: name=hello,value=world" =
  Http_cookie.create ~name:"hello" "world" |> pp_t ;
  [%expect
    {|
    name: hello
    value: world
    path:
    domain:
    expires:
    max_age:
    secure: false
    http_only: false
    same_site:
    extension: |}]

let%expect_test "create: name=he@llo" =
  Http_cookie.create ~name:"he@llo" "world" |> pp_t ;
  [%expect {| Error: cookie name: he@llo |}]

let%expect_test "create: name=he(llo" =
  Http_cookie.create ~name:"he(llo" "world" |> pp_t ;
  [%expect {| Error: cookie name: he(llo |}]

let%expect_test "create: name=he>llo" =
  Http_cookie.create ~name:"he>llo" "world" |> pp_t ;
  [%expect {| Error: cookie name: he>llo |}]

let%expect_test "create: name=he<llo" =
  Http_cookie.create ~name:"he<llo" "world" |> pp_t ;
  [%expect {| Error: cookie name: he<llo |}]

let%expect_test "create: value=val dd (space ' ' is invalid)" =
  Http_cookie.create ~name:"hello" "val dd" |> pp_t ;
  [%expect {| Error: cookie value: val dd |}]

let%expect_test "create: value=val,dd (',' is invalid)" =
  Http_cookie.create ~name:"hello" "val,dd" |> pp_t ;
  [%expect {| Error: cookie value: val,dd |}]
