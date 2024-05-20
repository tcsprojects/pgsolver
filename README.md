PGSolver
========

A collection of tools for generating, manipulating and - most of all - solving parity games.

Version 4.3, Copyright (c) 2008-2024, BSD 3 LICENSE

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.com)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Documentation

Please consult [```./doc/pgsolver.pdf```](https://github.com/tcsprojects/pgsolver/blob/master/doc/pgsolver.pdf) for a guide to installation, usage and development of this tool.


## Installation

Install the OCaml Package Manager [OPAM](https://opam.ocaml.org).

Then:
```bash	
opam update
opam upgrade
opam switch 4.07.0
eval `opam config env`
opam install ocamlbuild ocamlfind ounit TCSLib extlib ocaml-sat-solvers minisat
git clone https://github.com/tcsprojects/pgsolver.git
cd pgsolver
ocaml setup.ml -configure
ocaml setup.ml -build
```


## Examples

Run the exponential lower bound construction for Zadeh's rule on PGSolver:
```
  bin/stratimprgen -pg zadehexp [n] | bin/pgsolver -dsd -dgo -dlo -dsg -v 2 -jh -global policyiter -x -sfse
```
where [n] is the index of the game, e.g. 3.
