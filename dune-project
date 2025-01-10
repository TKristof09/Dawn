(lang dune 3.16)

(name dawn)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 3.0) 

(pin 
  (url "git+https://github.com/TKristof09/lrgrep")
  (package (name lrgrep)))

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
   lrgrep
   )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
