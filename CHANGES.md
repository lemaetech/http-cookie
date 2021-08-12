## v4.0.0 2021-08-12 

- [BREAKING-CHANGE] remove module `Same_site`. Use `same_site` type instead.
- [New] introduce type `same_site` to replace `Same_site` module and to better conform to RFC 6265.
- [New] add `of_cookie` features an angstrom parser to parse 'Cookie' header value as specified by RFC 6265.
- [New] add `pp`, `pp_date_time` and `pp_same_site` pretty prints type `t`, `date_time` and `same_site` respectively. Useful for debugging pruposes.
- [New] add `date_time` function to create valid `date_time` value.
- [BREAKING-CHANGE] make `date_time` type abstract
- [BREAKING-CHANGE] `to_set_cookie_header_value` has been removed. Use `to_set_cookie` instead.
- [BREAKING-CHANGE] `to_cookie_header_value` has been removed. Use `to_cookie` instead.
- [BREAKINg-CHANGE] `of_cookie_header` has been removed, Use`of_cookie` instead.

## v3.1.0 2021-08-11 

- Add `update_*` functions.

## v3.0.0 2020-24-12 UK

- Backwards incompatible change: remove `base-unix` dependency. Uses own `date_time` instead of `Unix.tm`.

## v2.0.0 2020-09-09 UK

- Rewrite library by removing almost all ppx and external libraries
- Rename package to `http-cookie` from `cookies`.
- Make the `Http_cookie` the topmost module.
- Change code formatting to `janestreet`
- Document API, generate docs and host it.

## v1.0.1 2020-09-09 UK

- Add compare function

## v1.0.0 2020-08-01 UK

- First release.
