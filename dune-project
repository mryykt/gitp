(lang dune 3.7)

(name gitp)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name gitp)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml (>= 4.10.0))
  dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
