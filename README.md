PGSolver
========

Version 4.0, Copyright (c) 2008-2017

It is developed and maintained by:
- (c) Oliver Friedmann, University of Munich (http://oliverfriedmann.com)
- (c) Martin Lange, University of Kassel (http://carrick.fmv.informatik.uni-kassel.de/~mlange/)


## Documentation

Please consult [```./doc/pgsolver.pdf```](https://github.com/tcsprojects/pgsolver/blob/master/doc/pgsolver.pdf) for a guide to installation, usage and development of this tool.


## Installation

Install OCaml, OUnit, OPAM, Ocamlbuild.

Then:
```bash	
git clone https://github.com/tcsprojects/pgsolver.git
cd pgsolver
git submodule update --init
make
```


### Sat Solvers

#### Picosat

```bash	
wget http://fmv.jku.at/picosat/picosat-965.tar.gz
tar xzvf picosat-965.tar.gz
cd picosat-965
./configure.sh && make
cd ..
echo "PICOSAT = `pwd`/picosat-965/libpicosat.a" >> pgsolver/SatConfig
```
#### ZChaff

```bash	
wget https://www.princeton.edu/~chaff/zchaff/zchaff.64bit.2007.3.12.zip
tar xzvf zchaff.64bit.2007.3.12.zip 
cd zchaff64
make
cd ..
echo "ZCHAFF = `pwd`/zchaff64/libsat.a" >> pgsolver/SatConfig
```

#### MiniSat

```bash
git clone https://github.com/niklasso/minisat
cd minisat
make
cd ..
echo "MINISAT = `pwd`/minisat/build/release/lib/libminisat.a" >> pgsolver/SatConfig
echo "MINISAT_INC = `pwd`/minisat" >> pgsolver/SatConfig
```

If you're on a Mac and make fails, please have a look at these resources:
- https://github.com/u-u-h/minisat/commit/e768238f8ecbbeb88342ec0332682ca8413a88f9
- http://web.cecs.pdx.edu/~hook/logicw11/Assignments/MinisatOnMac.html
