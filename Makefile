# SMTMODULES=-ccopt "-I$(Z3DIR)/ocaml -L$(Z3DIR)/ocaml -L$(Z3DIR)/lib" -cclib -lz3 $(OCAML_DIR)/libcamlidl.a $(Z3DIR)/ocaml/z3.$(COMPILELIBEXT)
# SAT?


all: pgsolver generators tools test


pgsolver:
	ocamlbuild -libs nums main.native
	mv main.native bin/pgsolver

test:
	ocamlbuild -libs nums -package oUnit solverstest.native
	mv solverstest.native bin/ounit


#############################################
## create the generators
#############################################

generators: randomgame.gen laddergame.gen clusteredrandomgame.gen cliquegame.gen modelcheckerladder.gen recursiveladder.gen steadygame.gen jurdzinskigame.gen elevatorverification.gen towersofhanoi.gen langincl.gen stratimprgen.gen recursivedullgame.gen

%.gen:
	ocamlbuild -libs nums $*_aux.native
	mv $*_aux.native bin/$*



#############################################
## create the tools
#############################################

tools: auso.tool benchmark.tool benchstratimpr.tool combine.tool complexdecomp.tool compressor.tool fullimprarena.tool imprarena.tool infotool.tool obfuscator.tool policyitervis.tool transformer.tool winningstrats.tool itersat.tool

imprarena.tool: 
	ocamlbuild -libs nums -libs str imprarena.native
	mv imprarena.native bin/imprarena

%.tool:
	ocamlbuild -libs nums $*.native
	mv $*.native bin/$*


#############################################
## clean up the compilation directory
#############################################
clean:
	ocamlbuild -clean
