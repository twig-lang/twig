(lang dune 3.19)

(name twig)

(generate_opam_files true)

(source (github nqr2/twig))

(authors "nqr2 <naqr2@proton.me>")
(maintainers "nqr2 <naqr2@proton.me>")

(license LICENSE)

(using menhir 3.0)

(package
 (name twig)
 (depends ocaml sedlex menhirLib)
 (tags ()))
