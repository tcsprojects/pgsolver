(lang dune 3.18)
(name pgsolver)
(version 4.4)

(generate_opam_files true)

(source (github tcsprojects/pgsolver))

(package
 (name pgsolver)
 (maintenance_intent "(latest)")
 (synopsis "A collection of tools for generating, manipulating and - most of all - solving parity games")
 (description "A collection of tools for generating, manipulating and - most of all - solving parity games.")
 (depends
   (ocaml (>= 4.8))
   dune
   num
   (tcs-lib (>= 0.6))
   extlib
   (ocaml-sat-solvers (>= 0.8))
 )
 (license "BSD-3-clause")
 (authors "Oliver Friedmann" "Martin Lange") 
 (maintainers "Oliver Friedmann" "Martin Lange")
 (homepage "https://github.com/tcsprojects/pgsolver")
 (bug_reports "https://github.com/tcsprojects/pgsolver/issues")
)
