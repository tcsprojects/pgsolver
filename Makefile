# SMTMODULES=-ccopt "-I$(Z3DIR)/ocaml -L$(Z3DIR)/ocaml -L$(Z3DIR)/lib" -cclib -lz3 $(OCAML_DIR)/libcamlidl.a $(Z3DIR)/ocaml/z3.$(COMPILELIBEXT)
# SAT?


all: pgsolver generators tools test



pgsolver:
	ocamlbuild -libs nums main.native
	mv main.native bin/pgsolver



test:
	ocamlbuild -libs nums -package oUnit solverstest.native
	mv solverstest.native bin/ounit
	


generators: randomgame laddergame clusteredrandomgame cliquegame modelcheckerladder recursiveladder steadygame jurdzinskigame elevatorverification towersofhanoi langincl stratimprgen


randomgame:
	echo "open Randomgame;; open Rungenerator;;" > temprandomgame.ml
	ocamlbuild -libs nums temprandomgame.native
	mv temprandomgame.native bin/randomgame
	rm temprandomgame.ml
	
laddergame:
	echo "open Laddergame;; open Rungenerator;;" > templaddergame.ml
	ocamlbuild -libs nums templaddergame.native
	mv templaddergame.native bin/laddergame
	rm templaddergame.ml
	
clusteredrandomgame:
	echo "open Clusteredrandomgame;; open Rungenerator;;" > tempclusteredrandomgame.ml
	ocamlbuild -libs nums tempclusteredrandomgame.native
	mv tempclusteredrandomgame.native bin/clusteredrandomgame
	rm tempclusteredrandomgame.ml

cliquegame:
	echo "open Cliquegame;; open Rungenerator;;" > tempcliquegame.ml
	ocamlbuild -libs nums tempcliquegame.native
	mv tempcliquegame.native bin/cliquegame
	rm tempcliquegame.ml

modelcheckerladder:
	echo "open Modelcheckerladder;; open Rungenerator;;" > tempmodelcheckerladder.ml
	ocamlbuild -libs nums tempmodelcheckerladder.native
	mv tempmodelcheckerladder.native bin/modelcheckerladder
	rm tempmodelcheckerladder.ml

recursiveladder:
	echo "open Recursiveladder;; open Rungenerator;;" > temprecursiveladder.ml
	ocamlbuild -libs nums temprecursiveladder.native
	mv temprecursiveladder.native bin/recursiveladder
	rm temprecursiveladder.ml

steadygame:
	echo "open Steadygame;; open Rungenerator;;" > tempsteadygame.ml
	ocamlbuild -libs nums tempsteadygame.native
	mv tempsteadygame.native bin/steadygame
	rm tempsteadygame.ml

jurdzinskigame:
	echo "open Jurdzinskigame;; open Rungenerator;;" > tempjurdzinskigame.ml
	ocamlbuild -libs nums tempjurdzinskigame.native
	mv tempjurdzinskigame.native bin/jurdzinskigame
	rm tempjurdzinskigame.ml

elevatorverification:
	echo "open Elevatorverification;; open Rungenerator;;" > tempelevatorverification.ml
	ocamlbuild -libs nums tempelevatorverification.native
	mv tempelevatorverification.native bin/elevatorverification
	rm tempelevatorverification.ml

towersofhanoi:
	echo "open Towersofhanoi;; open Rungenerator;;" > temptowersofhanoi.ml
	ocamlbuild -libs nums temptowersofhanoi.native
	mv temptowersofhanoi.native bin/towersofhanoi
	rm temptowersofhanoi.ml

langincl:
	echo "open Langincl;; open Rungenerator;;" > templangincl.ml
	ocamlbuild -libs nums templangincl.native
	mv templangincl.native bin/langincl
	rm templangincl.ml

stratimprgen:
	echo "open Stratimprgen;; open Rungenerator;;" > tempstratimprgen.ml
	ocamlbuild -libs nums tempstratimprgen.native
	mv tempstratimprgen.native bin/stratimprgen
	rm tempstratimprgen.ml



tools: auso benchmark benchstratimpr combine complexdecomp compressor fullimprarena imprarena infotool obfuscator policyitervis transformer winningstrats itersat



obfuscator:
	ocamlbuild -libs nums obfuscator.native
	mv obfuscator.native bin/obfuscator

transformer:
	ocamlbuild -libs nums transformer.native
	mv transformer.native bin/transformer

compressor:
	ocamlbuild -libs nums compressor.native
	mv compressor.native bin/compressor

combine:
	ocamlbuild -libs nums combine.native
	mv combine.native bin/combine

infotool:
	ocamlbuild -libs nums infotool.native
	mv infotool.native bin/infotool
	
winningstrats:	
	ocamlbuild -libs nums winningstrats.native
	mv winningstrats.native bin/winningstrats
	
benchmark: 
	ocamlbuild -libs nums benchmark.native
	mv benchmark.native bin/benchmark

imprarena: 
	ocamlbuild -libs nums -libs str imprarena.native
	mv imprarena.native bin/imprarena
	
fullimprarena: 
	ocamlbuild -libs nums fullimprarena.native
	mv fullimprarena.native bin/fullimprarena
	
auso: 
	ocamlbuild -libs nums auso.native
	mv auso.native bin/auso
	
complexdecomp:
	ocamlbuild -libs nums complexdecomp.native
	mv complexdecomp.native bin/complexdecomp

policyitervis:
	ocamlbuild -libs nums policyitervis.native
	mv policyitervis.native bin/policyitervis

benchstratimpr:
	ocamlbuild -libs nums benchstratimpr.native
	mv benchstratimpr.native bin/benchstratimpr

itersat:
	ocamlbuild -libs nums itersat.native
	mv itersat.native bin/itersat
	