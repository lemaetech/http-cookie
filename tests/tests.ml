let () = Printexc.record_backtrace false
let print_date_time r ok = Fmt.result ~ok ~error:Fmt.string Format.std_formatter r

let%expect_test "date_time: year 0 " =
  let dt =
    Http_cookie.date_time ~year:0 ~month:`Jan ~weekday:`Sun ~day_of_month:23
      ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid year (>0 && < 9999): 0 |}]

let%expect_test "date_time: year 10000" =
  let dt =
    Http_cookie.date_time ~year:10_000 ~month:`Jan ~weekday:`Sun
      ~day_of_month:23 ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid year (>0 && < 9999): 10000 |}]

let%expect_test "date_time: year 2021" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:23
      ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 23 Jan 2021 22:45:59 GMT |}]

let%expect_test "date_time: day_of_month 0" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:0
      ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid day of month ( > 0 && <= 31): 0 |}]

let%expect_test "date_time: day_of_month 31" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:31
      ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 31 Jan 2021 22:45:59 GMT |}]

let%expect_test "date_time: day_of_month 32" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:32
      ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid day of month ( > 0 && <= 31): 32 |}]

(* hour tests *)
let%expect_test "date_time: hour 0" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:0 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid hour (>0 && <24): 0 |}]

let%expect_test "date_time: hour 24" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:24 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid hour (>0 && <24): 24 |}]

let%expect_test "date_time: hour 22" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:45 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:45:59 GMT |}]

(* minute tests *)
let%expect_test "date_time: minute 0" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:0 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:00:59 GMT |}]

let%expect_test "date_time:minute 24" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:24 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:59 GMT |}]

let%expect_test "date_time: minute 60" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:60 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid minutes (>=0 && < 60): 60 |}]

(* seconds test *)
let%expect_test "date_time: seconds 0" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:24 ~seconds:0
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:00 GMT |}]

let%expect_test "date_time: seconds 59" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:24 ~seconds:59
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Sun, 30 Jan 2021 22:24:59 GMT |}]

let%expect_test "date_time: seconds 60" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:24 ~seconds:60
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid seconds (>=0 && < 60): 60 |}]

let%expect_test "date_time: seconds -1" =
  let dt =
    Http_cookie.date_time ~year:2021 ~month:`Jan ~weekday:`Sun ~day_of_month:30
      ~hour:22 ~minutes:24 ~seconds:(-1)
  in
  print_date_time dt Http_cookie.pp_date_time ;
  [%expect {| Invalid seconds (>=0 && < 60): -1 |}]

(* create tests *)


