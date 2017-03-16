# TODO: SMTMODULES=-ccopt "-I$(Z3DIR)/ocaml -L$(Z3DIR)/ocaml -L$(Z3DIR)/lib" -cclib -lz3 $(OCAML_DIR)/libcamlidl.a $(Z3DIR)/ocaml/z3.$(COMPILELIBEXT)

# WITH_PROFILING = YES

ifdef WITH_PROFILING
	BUILDEXT = p.native
else
	BUILDEXT = native
endif

LIBS = nums,str

all: pgsolver generators tools test

include satsolversforocaml/SatCompile

pgsolver: generatesat
	ocamlbuild $(SATFLAGS) -libs $(LIBS) -package extlib main.$(BUILDEXT)
	mv main.$(BUILDEXT) bin/pgsolver

test: generatesat
	ocamlbuild $(SATFLAGS) -libs $(LIBS) -package oUnit -package extlib solverstest.$(BUILDEXT)
	mv solverstest.$(BUILDEXT) bin/ounit


#############################################
## create the generators
#############################################

generators: randomgame.gen laddergame.gen clusteredrandomgame.gen cliquegame.gen modelcheckerladder.gen recursiveladder.gen steadygame.gen jurdzinskigame.gen elevators.gen towersofhanoi.gen langincl.gen stratimprgen.gen recursivedullgame.gen

%.gen:
	ocamlbuild -libs $(LIBS) $*_aux.$(BUILDEXT)
	mv $*_aux.$(BUILDEXT) bin/$*



#############################################
## create the tools
#############################################

tools: auso.tool benchmark.tool benchstratimpr.tool combine.tool complexdecomp.tool compressor.tool fullimprarena.tool imprarena.tool infotool.tool obfuscator.tool policyitervis.tool transformer.tool winningstrats.tool itersat.tool

%.tool: generatesat
	ocamlbuild $(SATFLAGS) -libs $(LIBS) -package extlib $*.$(BUILDEXT)
	mv $*.$(BUILDEXT) bin/$*


#############################################
## clean up the compilation directory
#############################################
clean:
	ocamlbuild -clean
