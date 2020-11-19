# Cookies

A comprehensive and standards compliant HTTP cookies library for ocaml.

HTTP cookies are serialized as follows:

- In a `Cookie` header in a HTTP request
- In a `Set-Cookie` header in a HTTP response.

The library supports consuming and creating HTTP cookies in both requests
and responses.

The standard implemented by the library is [RFC 6265](https://tools.ietf.org/html/rfc6265).

[API Documentation](https://lemaetech.co.uk/http-cookies/)

## Installation

```sh
$ opam install http-cookies
```
