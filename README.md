PGSolver
========

A collection of tools for generating, manipulating and - most of all - solving parity games.

Version 4.4, Copyright (c) 2008-2025, BSD 3 LICENSE

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.com)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Documentation

Please consult [```./doc/pgsolver.pdf```](https://github.com/tcsprojects/pgsolver/blob/master/doc/pgsolver.pdf) for a guide to installation, usage and development of this tool.


## OPAM

You can install this package via OPAM under the name `pgsolver`.


## Commands


### Build

```
dune build
```

### Release

1. Change version in `dune-project`.
2. Update `CHANGES.md`.
3. Run `dune build`.
4. Commit
```
  git status
  git add -A
  git commit -m "message"
  git tag v0.x [--force]
  git push origin master --tags [--force]
```
5. Release
```
  dune-release tag
  dune-release distrib
  dune-release publish
  dune-release opam pkg
  dune-release opam submit
```  


## Examples

Generate a parity game using the generator app by running the following from
the directory where you did `dune build`:
```
  ./_build/default/src/apps/generator/generator.exe --generator recursiveladder 4
```
Generate a parity game using Friedmann's exponential lower-bound family of
examples for strategy improvement:
```
  ./_build/default/src/apps/generators/stratimprgen.exe -pg friedmannsubexp [n]
```

