(lang dune 2.6)

(name cookies)

(generate_opam_files true)

(source
 (github lemaetech/cookies))

(license MPL-2.0)

(authors "Bikal Lem <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name cookies)
 (synopsis "HTTP cookies library for OCaml")
 (description
   "OCaml library to manipulate HTTP cookies. Adheres to RFC 6265.")
 (depends
  (ocaml
   (>= 4.10.0))
  dune
  base-unix))
