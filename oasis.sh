#!/bin/sh
oasis setup
ocaml setup.ml -configure
ocaml setup.ml -build
