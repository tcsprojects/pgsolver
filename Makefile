# WITH_PROFILING = YES

ifdef WITH_PROFILING
	BUILDEXT = p.native
else
	BUILDEXT = native
endif

LIBS = nums,str
PACKAGES = -package extlib -package TCSLib -package ocaml-sat-solvers

all: pgsolver generators tools test

pgsolver:
	ocamlbuild -libs $(LIBS) $(PACKAGES) main.$(BUILDEXT)
	mv main.$(BUILDEXT) bin/pgsolver

test:
	ocamlbuild -libs $(LIBS) -package oUnit $(PACKAGES) solverstest.$(BUILDEXT)
	mv solverstest.$(BUILDEXT) bin/ounit


#############################################
## create the generators
#############################################

generators: randomgame.gen laddergame.gen clusteredrandomgame.gen cliquegame.gen modelcheckerladder.gen recursiveladder.gen steadygame.gen jurdzinskigame.gen elevators.gen towersofhanoi.gen langincl.gen stratimprgen.gen recursivedullgame.gen

%.gen:
	ocamlbuild -libs $(LIBS) $(PACKAGES) $*_aux.$(BUILDEXT)
	mv $*_aux.$(BUILDEXT) bin/$*



#############################################
## create the tools
#############################################

tools: auso.tool benchmark.tool benchstratimpr.tool combine.tool complexdecomp.tool compressor.tool fullimprarena.tool imprarena.tool infotool.tool obfuscator.tool policyitervis.tool transformer.tool winningstrats.tool itersat.tool

%.tool:
	ocamlbuild -libs $(LIBS) $(PACKAGES) $*.$(BUILDEXT)
	mv $*.$(BUILDEXT) bin/$*


#############################################
## clean up the compilation directory
#############################################
clean:
	ocamlbuild -clean
