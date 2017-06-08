#!/usr/bin/env bash
opam uninstall pgsolver
opam pin remove pgsolver
oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
oasis2opam --local -y
opam pin add pgsolver . -n -y
opam install pgsolver
