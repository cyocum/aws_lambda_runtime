(lang dune 3.20)

(name aws_lambda_runtime)

(generate_opam_files true)

(authors "Christina Yocum <christinayocum76@gmail.com")

(maintainers "Christina Yocum <christinayocum76@gmail.com")

(license MIT)

(package
 (name aws_lambda_runtime)
 (synopsis "A runtime for the AWS Lambda ecosystem for OCaml")
 (depends ocaml ezcurl base))
