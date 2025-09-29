(lang dune 3.10)
(name pgsolver)
(version 4.4)

(generate_opam_files true)

(source (github tcsprojects/pgsolver))

(package
 (name pgsolver)
 (synopsis "A collection of tools for generating, manipulating and - most of all - solving parity games")
 (description "A collection of tools for generating, manipulating and - most of all - solving parity games.")
 (depends
   (ocaml (>= 4.8))
   dune
   num
   tcs-lib
   extlib
   ocaml-sat-solvers
 )
 (license "BSD-3-clause")
 (authors "Oliver Friedmann" "Martin Lange") 
 (maintainers "Oliver Friedmann" "Martin Lange")
 (homepage "https://github.com/tcsprojects/pgsolver")
 (bug_reports "https://github.com/tcsprojects/pgsolver/issues")
)
