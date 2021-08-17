# Cookies

A comprehensive and strict standards compliant HTTP cookies library for ocaml. [API Documentation](https://lemaetech.co.uk/http-cookie/http-cookie/Http_cookie/index.html)

This library supports consuming and creating HTTP cookies found in HTTP request `Cookie` header and in `Set-Cookie` header in HTTP response. The library validates all cookie attributes, cookie name and cookie value for standards conformance and correct usage. The validation suite is comprehensive and includes validation of domain name, IPv4, IPv6 and HTTP date-time formats.

The RFC standards implemented by the library are:
- [Cookies - RFC 6265](https://tools.ietf.org/html/rfc6265) - Strict standards compliance. 
- [HTTP Date - RFC 1123](https://datatracker.ietf.org/doc/html/rfc1123) - HTTP date time format specification.
- [Domain Name - RFC 1034](https://datatracker.ietf.org/doc/html/rfc1034#section-3.5) - Domain name format specification.
- [Hosts - RFC 1123](https://datatracker.ietf.org/doc/html/rfc1123#section-2.1) - Host name format specification.
- [IPv4/IPv6](https://datatracker.ietf.org/doc/html/draft-main-ipaddr-text-rep-02#section-3}) - IPv4 and IPv6 address format specification.

## Installation

```sh
$ opam install http-cookie
```

## Usage

Create a cookie,

```ocaml
Http_cookie.create ~name:"SID" "23432324"
```

Serialize cookie to a HTTP `Set-Cookie` header value,

```ocaml
let s = Http_cookie.to_set_cookie c in
s = "SID=31d4d96e407aad42; Path=/; Secure; HttpOnly; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
```
