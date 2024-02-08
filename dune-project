(lang dune 3.12)

(name essentials_of_compilation)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name essentials_of_compilation)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base stdio fmt ppx_deriving ppx_inline_test)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
