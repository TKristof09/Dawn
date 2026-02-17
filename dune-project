(lang dune 3.21)

(name dawn)
(cram enable)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0) 

(package
 (name dawn)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml
   dune
   ppx_deriving
   menhir
   core
   core_kernel
   lrgrep
   )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
