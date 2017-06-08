Jun 3 2017      Oliver Friedmann
 - Use OASIS and OPAM instead of submodules and make

Aug 5 2016		Oliver Friedmann
 - Change Makefile to use Ocamlbuild instead

Jul 1 2016		Oliver Friedmann
 - Added unit tests

Jun 27 2016		Oliver Friedmann
 - Added support for generating MDP-like valuations

May 8 2014		Oliver Friedmann
 - Added tool for analyzing winning strategies

Sep 11 2013		Martin Lange
 - Added Fixpoint Iteration Algorithm

Aug 07 2013     Oliver Friedmann
 - Fixed a bug in the verification algorithm (thanks to Maks Verver for finding it!)

Jun 24 2013     Michael Falk
 - Added a new solver based on a reduction to mu-calculus model checking

Jun 6 2013      Martin Lange
 - Added a functor Build in src/paritygame/paritygame.ml which turns a datatype 
   for paritygame nodes into a function that creates a parity game from some
   initial node given functions that create the successors etc. of a node.
 - Added a new benchmark "langincl" formalising an inclusion problem between
   an NBA and a DBA as a solitaire game of index 3. See this for the use of the 
   functor Build.

Apr 26 2013     Oliver Friedmann
 - Added a parity game to abstract unique sink / grid unique sink orientations
   called "auso"

Oct 16 2012		Oliver Friedmann
 - You can now compile PGSolver with ocamlc; just create copies of all three
   (./, ./satsolvers, ./tcslib) "Config.default" and name them "Config"; set
   "COMPILE_WITH_OPT=NO"

May 17 2012		Oliver Friedmann
 - Added visualization scripts for strategy iteration
 - Changed the strategy iteration lower bound generators into one big generating tool

Apr 30 2012		Oliver Friedmann
 - Updated Generators
 - Added LP'fication tool
 - Added Improvement Arena tool

Feb 24 2012		Oliver Friedmann
 - Changed inclusion of config files
 - Renamed "Libversion" to "Libpgsolver"

May 08 2011		Oliver Friedmann
 - Added round-robin lower bound generator

Apr 28 2011		Martin Lange
 - Added smt-based solver

Jan 23 2011		Oliver Friedmann
 - Added lower bound generator for Fearnley's non-oblivious rule

Jan 05 2011		Oliver Friedmann
 - Solvers are now parameterized via the command line
 - All strategy improvement solvers are now seen as one solver
 
Dec 02 2010		Martin Lange
 - Added Genetic Solver

Nov 28 2010		Oliver Friedmann
 - Changed definition of partial solvers
 - Fixed minor bug in stratimprlocal

Feb 02 2010		Oliver Friedmann
 - Removed Recursive Preservation Algorithm (it was just experimental
   and didn't prove to be a real improvement...)
 - Removed Force Reach Algorithm (that was an experimental incomplete
   algorithm and it wasn't good either)

Jan 17 2010		Oliver Friedmann
 - New Generator Towers Of Hanoi
 - New Solver Strategy Iteration by Reduction to DPG
 - New Solver Vorobyov's Randomized Approach #1
 - New Solver Vorobyov's Randomized Approach #2
 - New Genuine Solver Local Strategy Improvement
 - Improved Solver Implementation Model Checker by Stevens / Stirling
 - Outsourced almost all utility functions and modules to TCSLib
 - Enabled Local Solving in PGSolver and Benchmark
 - Library Version of PGSolver
 
 
Mar 28 2009		Oliver Friedmann
 - Added GeneratorList containing a list of all generators; Makefile
   now automatically compiles these generators.
 - Added new generator expstratvoege which is a simplified version
   of expstratimpr enforcing exponential behaviour w.r.t. Voege and Puri.


Mar 27 2009		Martin Lange
 - Added .depend compilation
 - Added new elevator verification generator
 - Jurdzinski Generator is currently broken.


Mar 23 2009		Oliver Friedmann
 - Removed external solver for linear inequality systems. Created a
   very slow but exact solution for dpgs. Megiddo's algorithm for
   solving inequality systems should be implemented.
   Note: Exponential Strategy Improvement Examples also work with
   Puri's Algorithm.


Mar 22 2009		Oliver Friedmann
 - Added experimental solver using the standard reduction to DPG (experimental).
   It requires GLPK as well as OCAMLGLPK to be compiled into PGSolver.
   Unfortunately, floating point precision only suffices for very very very
   small games. Therefore, this algorithm is practically unusable in this
   context. One should implement another version relying on exact rational
   numbers; since GLPK is not able to exact parameters other than float,
   one has to implement a native linear inequality solver. Megiddo's
   algorithm should suffice.


Mar 20 2009		Oliver Friedmann
 - Improved Small Progress Measures Implementation: It now computes the SPM for both
   players simultaneously and checks after a number of steps if there is already a
   stable dominion and if so, the respective spms of the other player are marked as top.


Mar 17 2009		Oliver Friedmann
 - Enriched Arg2-Module s.t. sub-parsing is supported
 - Added support for generators in PGSolver; use LINKGENERATORS=YES in Config
   file to enable linking of generators into PGSolver

 
Mar 10 2009		Oliver Friedmann
 - Added SATSolversForOcaml/SatSolverMods which should be included in Makefiles using Sat Solvers
 - Added PGSolver/SolverList which should be included in Makefiles using Parity Game Solvers


Mar 08 2009		Oliver Friedmann
 - Renamed expstratimpr --> superpolystratimpr
 - Added new expstratimpr with linear number of edges


Mar 02 2009		Oliver Friedmann
 - Removed Benchmarks, Preciousgames, demos and satsolvers subdirectory
 - Outsourced satsolvers as separate package
 - Outsourced mlsolver as separate package
 - Added printing of parsable solution and strategy
 - Added parsing of parsable solution and strategy


Feb 26 2009		Oliver Friedmann
 - Moved global fixed point algorithm to temporary folder


Feb 25 2009		Oliver Friedmann
 - Added global fixed point algorithm; generated strategy is NOT correct ATM
 - Fixed Makefiles s.t. it really compiles without gcc


Feb 12 2009		Oliver Friedmann
 - Updated configuration files s.t. SATSolvers have to be configured only in the respective config file
 - Updated documentation as well


Feb 11 2009		Martin Lange
 - Added min-party / max-parity coversion
 - Node: jurdzinskigame generates min-parity games!


Jan 28 2009		Oliver Friedmann
 - Added strategy improvement SAT reduction


Jan 26 2009		Oliver Friedmann
 - Added install.txt
 - Updated documentation w.r.t. parser
 - Updated tools w.r.t. parser
 - Added Parserhelper.parse_from_channel: in_channel -> bool -> Paritygame.paritygame
   The second parameter enables / disables sanity checking
 - Parser: Removed parser routines from Paritygame.ml (and moved them to parser.mly)
 - Model Checker: Now it returns an underapproximation instead of a solution to the whole scc. Major speed up.


Jan 25 2009		Oliver Friedmann
 - Updated documentation: Model Checker Generator, Obfuscator
 - Added Model Checker worst-case generator
 - Obfuscator: Fixed edge obfuscation, New command line parameters.
 - Model Checker: A bit more output


Jan 24 2009		Oliver Friedmann
 - Documentation Update: Benchmarks, Developer's Guide


Jan 22 2009		Oliver Friedmann
 - Documentation Update: Combine Tool, Transformer, Benchmark Tool
 - Added Combine Tool
 - Added Transformations.combine_games: paritygame list -> paritygame
 - Tested compiling and integrating SAT solvers on a clean machine and it worked. It's a miracle!
 - Documentation Update: Installation, Compilation, Supported Sat Solvers, Parameters of PGSolver, Invocation Output, Specification, Compressor, Obfuscator


Jan 21 2009		Oliver Friedmann
 - Enhanced Clustered Random Generator
 - Renamed GameBased Algorithm to LocalModelChecker
 - Improved Attractor Computation


Jan 21 2009		Martin Lange
 - Added Clique Game


Jan 20 2009		Oliver Friedmann
 - Documentation Update: Updated intro, algorithms, License
 - Added version info (src/pgsolver/info.ml)
 - Updated tool and generator output


Jan 19 2009		Oliver Friedmann
 - Altered parser s.t. it reads both formats
 - Removed local config files from repository
 - Added changelog.txt, will be included in make package
 - Added Makefile commands
 	- satsolvers: Compiles Sat Solvers
 	- cleansat: Cleans Sat Solvers
 	- cleanall: Cleans everything and sat solvers
 - Modified Makefile commands
 	- all, pgsolver: Call now automatically satsolvers
 - Added Config.default and satsolvers/Config.default disabling by default
   all external sat solvers
 - Modified Makefile and satsolvers/Makefile s.t. Config.default is used iff
   Config is not existing
 - Updated make package. Includes default config files. Creates a pgsolver
   sub-directory. Tested it on a clean machine without any sat solver and
   it worked.
 - All tools, generators etc. should output the new format of parity games.
 - Removed useless generators: Centeredcycleinvariantdemo, Stratimprovelift,
   Succcollapse


Jan 16 2009		Martin Lange
 - Altered random generation: See src/tools/randomutils.ml
   val get_pairwise_different_from_range : int -> int -> int -> int list
 - Altered parser: Parity games now need to start with the line "parity n;"
   where n is supposed to be the heighest occurring node number. Increases
   parser performance.
