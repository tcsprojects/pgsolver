###########################################################
#
# INCLUDING CONFIGURATION FILE
#
###########################################################

ifeq ($(strip $(wildcard Config)),)
	include Config.default
else
	include Config
endif

ifeq "$(COMPILE_WITH_OPT)" "YES"

COMPILEEXT=cmx
OCAMLCOMP=$(OCAMLOPT)
COMPILELIBEXT=cmxa

else

COMPILEEXT=cmo
OCAMLCOMP=$(OCAMLC)
COMPILELIBEXT=cma

endif


SATSOLVERSOBJ=$(SATSOLVERSROOT)/obj

TCSLIBOBJ=$(TCSLIBROOT)/obj


ifeq "$(HASSMT)" "YES"

SMTMODULES=-ccopt "-I$(Z3DIR)/ocaml -L$(Z3DIR)/ocaml -L$(Z3DIR)/lib" -cclib -lz3 $(OCAML_DIR)/libcamlidl.a $(Z3DIR)/ocaml/z3.$(COMPILELIBEXT)

endif


###########################################################
#
# SETUP INCLUDES DIRECTIVE, SATSOLVERS AND COMPILER
#
###########################################################

INCLUDES=-I $(SRCDIR) -I $(OBJDIR) -I $(OCAML_DIR) -I $(SATSOLVERSOBJ) -I $(TCSLIBOBJ) -I $(Z3DIR)/ocaml

include $(SATSOLVERSROOT)/Config.include

# ifeq "$(HASSAT)" "YES"

CPPCOMPILER=-cc $(OCAMLOPTCPP)

# endif


###########################################################
#
# INCLUDE LIST OF SOLVERS
#
###########################################################

PGSOLVERSOBJ=$(OBJDIR)

include SolverList



###########################################################
#
# INCLUDE LIST OF GENERATORS
#
###########################################################

GENERATORSOBJ=$(OBJDIR)

include GeneratorList

ifeq "$(LINKGENERATORS)" "YES"

GENERATOR_MODULES=$(GENERATORS)

endif

SATSOLVERMODSX=$(SATSOLVERSOBJ)/satwrapper.$(COMPILEEXT) \
        $(SATSOLVERSOBJ)/satsolvers.$(COMPILEEXT)



MODULES_WITHOUT_SOLVERS_AND_LIB=$(OBJDIR)/basics.$(COMPILEEXT) \
        $(OBJDIR)/info.$(COMPILEEXT) \
	$(OBJDIR)/whoiswho.$(COMPILEEXT) \
	$(OBJDIR)/paritygame.$(COMPILEEXT) \
	$(OBJDIR)/mdp.$(COMPILEEXT) \
	$(OBJDIR)/transformations.$(COMPILEEXT) \
	$(OBJDIR)/specialsolve.$(COMPILEEXT) \
	$(OBJDIR)/univsolve.$(COMPILEEXT) \
	$(OBJDIR)/verification.$(COMPILEEXT) \
	$(OBJDIR)/solvers.$(COMPILEEXT) \
	$(OBJDIR)/generators.$(COMPILEEXT)

MODULES_WITHOUT_SOLVERS = $(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
	$(MODULES_WITHOUT_SOLVERS_AND_LIB)

MODULES=$(MODULES_WITHOUT_SOLVERS) \
	$(PGSOLVERS)

INTERFACES=$(MODULES:.$(COMPILEEXT)=.cmi)

EXECUTABLE=$(BINDIR)/pgsolver
LIBRARYNAME=$(OBJDIR)/libpgsolver.$(COMPILELIBEXT)

PACKAGE=pgsolver.tar

pgsolver: satsolvers $(INTERFACES) $(MODULES) $(GENERATOR_MODULES) main exec

library: satsolvers $(INTERFACES) $(OBJDIR)/libpgsolver.cmi $(MODULES) $(OBJDIR)/libpgsolver.$(COMPILEEXT) libexec

all: pgsolver library generators tools

satsolvers: $(SATSOLVERSOBJ)/satsolvers.$(COMPILEEXT)

$(TCSLIBOBJ)/%.cmi:
	make -C $(TCSLIBROOT) all

$(TCSLIBOBJ)/%.$(COMPILELIBEXT):
	make -C $(TCSLIBROOT) all

$(SATSOLVERSOBJ)/satsolvers.$(COMPILEEXT):
	make -C $(SATSOLVERSROOT) all

main: $(OBJDIR)/main.$(COMPILEEXT)

exec:
	$(OCAMLCOMP) -o $(EXECUTABLE) $(CPPCOMPILER) nums.$(COMPILELIBEXT) $(SMTMODULES) $(SATSOLVERMODSX) $(SATSOLVERMODS) $(MODULES) $(GENERATOR_MODULES) $(OBJDIR)/main.$(COMPILEEXT)

libexec:
	$(OCAMLCOMP) -a -o $(LIBRARYNAME) $(CPPCOMPILER) $(MODULES_WITHOUT_SOLVERS_AND_LIB) $(PGSOLVERS) $(OBJDIR)/libpgsolver.$(COMPILEEXT)

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/pgsolver/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/generators/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/generators/stratimpr/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/generators/policyiter/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/generators/policyiter/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/generators/policyiter/generators/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/tools/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/paritygame/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/solvers/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.$(COMPILEEXT): $(SRCDIR)/%.ml
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/paritygame/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/solvers/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/pgsolver/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

$(OBJDIR)/%.cmi: $(SRCDIR)/%.mli
	$(OCAMLCOMP) $(INCLUDES) -c -o $@ $<

whoiswho: $(SRCDIR)/pgsolver/whoiswho.ml

$(SRCDIR)/pgsolver/whoiswho.ml: $(SRCDIR)/pgsolver/encipher.ml
	$(OCAML) $< > $@

generators: $(GENERATORS:$(OBJDIR)/%.$(COMPILEEXT)=%) stratimprgen

tools: obfuscator transformer compressor combine benchmark infotool

%: $(SRCDIR)/generators/%.ml $(OBJDIR)/%.$(COMPILEEXT) $(OBJDIR)/rungenerator.$(COMPILEEXT)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/$@ nums.$(COMPILELIBEXT) $(OBJDIR)/info.$(COMPILEEXT) $(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) $(OBJDIR)/paritygame.$(COMPILEEXT) $(OBJDIR)/generators.$(COMPILEEXT) $(OBJDIR)/$@.$(COMPILEEXT) $(OBJDIR)/rungenerator.$(COMPILEEXT)

%: $(SRCDIR)/generators/stratimpr/%.ml $(OBJDIR)/%.$(COMPILEEXT) $(OBJDIR)/rungenerator.$(COMPILEEXT)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/$@ nums.$(COMPILELIBEXT) $(OBJDIR)/info.$(COMPILEEXT) $(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) $(OBJDIR)/paritygame.$(COMPILEEXT) $(OBJDIR)/generators.$(COMPILEEXT) $(OBJDIR)/$@.$(COMPILEEXT) $(OBJDIR)/rungenerator.$(COMPILEEXT)

STRATIMPR_LOWERBOUNDS=$(OBJDIR)/zadehexp.$(COMPILEEXT) \
                     $(OBJDIR)/cunninghamexp.$(COMPILEEXT) \
                     $(OBJDIR)/zadehsubexp.$(COMPILEEXT) \
                     $(OBJDIR)/cunninghamsubexp.$(COMPILEEXT) \
                     $(OBJDIR)/fearnleysubexp.$(COMPILEEXT) \
                     $(OBJDIR)/friedmannsubexp.$(COMPILEEXT) \
                     $(OBJDIR)/switchallsubexp.$(COMPILEEXT) \
                     $(OBJDIR)/switchbestsubexp.$(COMPILEEXT) \
                     $(OBJDIR)/switchallexp.$(COMPILEEXT) \
                     $(OBJDIR)/switchbestexp.$(COMPILEEXT) \
                     $(OBJDIR)/randomfacetsubexp.$(COMPILEEXT) \
                     $(OBJDIR)/randomedgesubexp.$(COMPILEEXT) \
                     $(OBJDIR)/randomedgeexptest.$(COMPILEEXT)


STRATIMPRGEN_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                     $(OBJDIR)/basics.$(COMPILEEXT) \
                     $(OBJDIR)/info.$(COMPILEEXT) \
                     $(OBJDIR)/paritygame.$(COMPILEEXT) \
                     $(OBJDIR)/mdp.$(COMPILEEXT) \
                     $(OBJDIR)/stratimprgenerators.$(COMPILEEXT) \
                     $(STRATIMPR_LOWERBOUNDS) \
                     $(OBJDIR)/stratimprgen.$(COMPILEEXT)


stratimprgen: $(INTERFACES) $(OBJDIR)/stratimprgenerators.cmi $(STRATIMPRGEN_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) nums.$(COMPILELIBEXT) -o $(BINDIR)/stratimprgen $(STRATIMPRGEN_MODULES)


OBFUSCATOR_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/obfuscator.$(COMPILEEXT)

obfuscator: $(INTERFACES) $(OBFUSCATOR_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/obfuscator $(OBFUSCATOR_MODULES)

COMBINE_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/combine.$(COMPILEEXT) \

combine: $(INTERFACES) $(COMBINE_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/combine $(COMBINE_MODULES)

INFOTOOL_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/infotool.$(COMPILEEXT) \

infotool: $(INTERFACES) $(INFOTOOL_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/infotool $(INFOTOOL_MODULES)

IMPRARENA_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/solvers.$(COMPILEEXT) \
                   $(OBJDIR)/specialsolve.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/univsolve.$(COMPILEEXT) \
                   $(OBJDIR)/stratimpralgs.$(COMPILEEXT) \
                   $(OBJDIR)/imprarena.$(COMPILEEXT) \

imprarena: $(INTERFACES) $(IMPRARENA_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) str.$(COMPILELIBEXT) -o $(BINDIR)/imprarena $(IMPRARENA_MODULES)

FULLIMPRARENA_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/solvers.$(COMPILEEXT) \
                   $(OBJDIR)/specialsolve.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/univsolve.$(COMPILEEXT) \
                   $(OBJDIR)/stratimpralgs.$(COMPILEEXT) \
                   $(OBJDIR)/fullimprarena.$(COMPILEEXT) \

fullimprarena: $(INTERFACES) $(FULLIMPRARENA_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) str.$(COMPILELIBEXT) -o $(BINDIR)/fullimprarena $(FULLIMPRARENA_MODULES)

AUSO_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/solvers.$(COMPILEEXT) \
                   $(OBJDIR)/specialsolve.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/univsolve.$(COMPILEEXT) \
                   $(OBJDIR)/stratimpralgs.$(COMPILEEXT) \
                   $(OBJDIR)/auso.$(COMPILEEXT) \

auso: $(INTERFACES) $(AUSO_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) str.$(COMPILELIBEXT) -o $(BINDIR)/auso $(AUSO_MODULES)

COMPLEXDECOMP_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/solvers.$(COMPILEEXT) \
                   $(OBJDIR)/specialsolve.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/univsolve.$(COMPILEEXT) \
                   $(OBJDIR)/stratimpralgs.$(COMPILEEXT) \
                   $(OBJDIR)/complexdecomp.$(COMPILEEXT) \

complexdecomp: $(INTERFACES) $(COMPLEXDECOMP_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) str.$(COMPILELIBEXT) -o $(BINDIR)/complexdecomp $(COMPLEXDECOMP_MODULES)

TRANSFORMER_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/transformer.$(COMPILEEXT)

transformer: $(INTERFACES) $(TRANSFORMER_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/transformer $(TRANSFORMER_MODULES)

ITERSAT_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
        $(OBJDIR)/basics.$(COMPILEEXT) \
		$(OBJDIR)/info.$(COMPILEEXT) \
		$(OBJDIR)/paritygame.$(COMPILEEXT) \
		$(SATSOLVERSOBJ)/satwrapper.$(COMPILEEXT) \
		$(SATSOLVERSOBJ)/satsolvers.$(COMPILEEXT) \
		$(SATSOLVERSOBJ)/pseudosatwrapper.$(COMPILEEXT) \
		$(SATSOLVERSOBJ)/preprocessor.$(COMPILEEXT) \
		$(SATSOLVERSOBJ)/externalsat.$(COMPILEEXT) \
		$(SATSOLVERMODS)

ITERSAT_INTERFACES=$(ITERSAT_MODULES:.$(COMPILEEXT)=.cmi)

itersat: satsolvers $(ITERSAT_INTERFACES) $(ITERSAT_MODULES) $(OBJDIR)/itersat.$(COMPILEEXT)
	$(OCAMLCOMP) -o $(BINDIR)/itersat $(CPPCOMPILER) $(ITERSAT_MODULES) $(OBJDIR)/itersat.$(COMPILEEXT)

benchmark: satsolvers $(INTERFACES) $(MODULES) $(OBJDIR)/benchmark.$(COMPILEEXT)
	$(OCAMLCOMP) $(LPMODULESCC) -o $(BINDIR)/benchmark $(CPPCOMPILER) nums.$(COMPILELIBEXT) $(SMTMODULES) $(SATSOLVERMODSX) $(SATSOLVERMODS) $(MODULES) $(OBJDIR)/benchmark.$(COMPILEEXT)

benchstratimpr: $(INTERFACES) $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.cmi $(OBJDIR)/stratimpralgs.$(COMPILEEXT) $(OBJDIR)/benchstratimpr.$(COMPILEEXT)
	$(OCAMLCOMP) -o $(BINDIR)/benchstratimpr $(CPPCOMPILER) nums.$(COMPILELIBEXT) $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.$(COMPILEEXT) $(OBJDIR)/benchstratimpr.$(COMPILEEXT)

policyitervis: $(INTERFACES) $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.cmi $(OBJDIR)/stratimpralgs.$(COMPILEEXT) $(OBJDIR)/policyitervis.$(COMPILEEXT)
	$(OCAMLCOMP) -o $(BINDIR)/policyitervis $(CPPCOMPILER) str.$(COMPILELIBEXT) nums.$(COMPILELIBEXT) $(MODULES_WITHOUT_SOLVERS) $(OBJDIR)/stratimpralgs.$(COMPILEEXT) $(OBJDIR)/policyitervis.$(COMPILEEXT)

COMPRESSOR_MODULES=$(TCSLIBOBJ)/tcslib.$(COMPILELIBEXT) \
                   $(OBJDIR)/info.$(COMPILEEXT) \
                   $(OBJDIR)/basics.$(COMPILEEXT) \
                   \
                   $(OBJDIR)/paritygame.$(COMPILEEXT) \
                   $(OBJDIR)/transformations.$(COMPILEEXT) \
                   $(OBJDIR)/compressor.$(COMPILEEXT)

compressor: $(INTERFACES) $(COMPRESSOR_MODULES)
	$(OCAMLCOMP) $(CPPCOMPILER) -o $(BINDIR)/compressor $(COMPRESSOR_MODULES)


clean:
	rm -f $(OBJDIR)/*.o $(OBJDIR)/*.cm*
	rm -f $(SRCDIR)/pgsolver/whoiswho.ml
	rm -f $(EXECUTABLE)

cleansat:
	make -C $(SATSOLVERSROOT) clean

veryclean: clean
	rm -f $(BINDIR)/randomgame $(BINDIR)/clusteredrandomgame $(BINDIR)/steadygame $(BINDIR)/recursiveladder $(BINDIR)/jurdzinskigame $(BINDIR)/laddergame $(BINDIR)/obfuscator $(BINDIR)/transformer $(BINDIR)/compressor $(BINDIR)/benchmark $(BINDIR)/itersat

cleanall: cleansat veryclean

package: whoiswho
	$(MAKE) -C doc
	$(TAR) cvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=$(SRCDIR)/temp/* --exclude=*~ --transform "s,^,pgsolver/," Makefile GeneratorList SolverList Config.default README install.txt changelog.txt src bin obj doc/pgsolver.pdf
	$(TAR) rvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,SATSolversForOCaml,pgsolver/satsolvers," $(SATSOLVERSROOT)/bin $(SATSOLVERSROOT)/obj $(SATSOLVERSROOT)/src $(SATSOLVERSROOT)/tester $(SATSOLVERSROOT)/Makefile $(SATSOLVERSROOT)/Config.default $(SATSOLVERSROOT)/Config.include $(SATSOLVERSROOT)/Solvers.default
	$(TAR) rvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=*~ --transform "s,TCSlib,pgsolver/TCSlib," $(TCSLIBROOT)/obj $(TCSLIBROOT)/src $(TCSLIBROOT)/Makefile $(TCSLIBROOT)/Config.default
	$(GZIP) $(PACKAGE)
#	$(TAR) czvf $(PACKAGE) --exclude=.svn --exclude=$(OBJDIR)/* --exclude=$(BINDIR)/* --exclude=satsolvers/$(OBJDIR)/* --exclude=satsolvers/$(BINDIR)/* --exclude=$(SRCDIR)/temp/* --exclude=*~ --exclude="$(SRCDIR)/pgsolver/encipher.*" --transform "s,^,pgsolver/," Makefile Config.default README install.txt src bin obj satsolvers/bin satsolvers/obj satsolvers/src satsolvers/tester satsolvers/Makefile satsolvers/Config.default doc/pgsolver.pdf

DEPENDENCY_INCLUDES=-I $(SRCDIR)/pgsolver \
                    -I $(SRCDIR)/generators \
                    -I $(SRCDIR)/tools \
                    -I $(SRCDIR)/paritygame \
                    -I $(SRCDIR)/solvers \
                    -I $(SRCDIR)

depend:
	$(OCAMLDEP) $(DEPENDENCY_INCLUDES) $(SRCDIR)/*/*ml $(SRCDIR)/*/*.mli | sed "s#$(SRCDIR)/[^/]*#$(OBJDIR)#g" > .depend

ifeq ($(strip $(wildcard .depend)),)
	include .depend
endif
