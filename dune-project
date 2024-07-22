(lang dune 3.15)

(name wyah)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github glyh/wyah))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/wyah)

(package
 (name wyah)
 (synopsis "Write You a Haskell implemented in OCaml")
 (description "Write You a Haskell implemented in OCaml")
 (depends
   ocaml
   dune
   menhir
   (nice_parser (>= 1.0.0))
   (ppx_deriving (>= 6.0.2))
   (linenoise (>= 1.5.1)))
          
 (tags
  (topics haskell compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
