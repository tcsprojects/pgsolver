PGSolver
========

Version 3.3, Copyright (c) 2008-2013

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.de)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Installation

- Install OCaml.
- Run "git submodule update --init" to checkout all required sub modules
- Create a copy of Config.default, name it Config and modify it to fit your configuration
- Run make


## Documentation

Please consult ```./doc/pgsolver.pdf``` for a guide to installation, usage and development of this tool.


## Config

There is one config file, ```./Config.default``` that has to be edited. It is highly recommended to create a copy of the file with the name Config in the respective directory that is to be edited instead of the original version. The Makefile checks whether a customized configuration file named Config exists and if so it is used instead of the default versions.

The configuration file starts with declarations about where to find all the programs necessary to build the executable PGSolver. Change these lines to point to the full path in which the OCaml compiler, lexer and parser generator are installed unless they are in the current PATH.

You need to give the full path of you OCaml installation directory: ```OCAML_DIR=/usr/lib/ocaml```
