# Cours "Sémantique et Application à la Vérification de programmes"
#
# Antoine Miné 2015
# Ecole normale supérieure, Paris, France / CNRS / INRIA

# Makefile


# points to compilers
#
# (we rely heavily on ocamlfind)
# (no change needed, the default ones in /usr/bin should be good)
# 
OCAMLFIND = ocamlfind
OCAMLLEX = ocamllex
MENHIR = menhir


# options for compilers
#
# (change this if you want to enable/disable debugging)
#
OCAMLCFLAGS = -g
OCAMLOPTFLAGS = -g
OCAMLLEXFLAGS =
MENHIRFLAGS = --explain


# library paths
#
# (change this if you add new subdirectories or libraries)
#
OCAMLINC = -I frontend -I libs -I domains -I iterator -I +zarith

# libraries
#
# (change this to add new libraries)
#
LIBS = -package zarith
CMXA =
CMA =

# name of the compiled executable
#
# (change to a better name matching your project)
#
TARGET = main


# compile either to byte code or native code
#
# uncomment only one of the two lines!
#
all: $(TARGET).byte
#all: $(TARGET).opt


# list of automatically generated files (by ocamllex and menhir)
#
# (this will probably not change during the project)
#
AUTOGEN = \
  frontend/lexer.ml \
  frontend/parser.ml \
  frontend/parser.mli


# list of ML source files to compile (including automatically generated)
#
# add your files here, in the right order!
#
MLFILES = \
  libs/mapext.ml \
  frontend/abstract_syntax_tree.ml \
  frontend/parser.ml \
  frontend/lexer.ml \
  frontend/cfg.ml \
  frontend/cfg_printer.ml \
  frontend/tree_to_cfg.ml \
  frontend/file_parser.ml \
  domains/domain.ml \
  domains/value_domain.ml \
  main.ml


# list of MLI source files
#
# add your files here
# (this is only used for ocamldep)
#
MLIFILES = \
  libs/mapext.mli \
  frontend/parser.mli \
  frontend/file_parser.mli


# below are general compilation rules
#
# you probably don't need to change anything below this point


# list of object files, derived from ML sources
#
CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)

$(TARGET).byte: $(CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ $(OCAMLCFLAGS) $(OCAMLINC) $(LIBS) $(CMA) -linkpkg $+

# native link
$(TARGET).opt: $(CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ $(OCAMLOPTFLAGS) $(OCAMLINC) $(LIBS) $(CMXA) -linkpkg $+


# compilation rules

%.cmo: %.ml %.cmi
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLINC) $(LIBS) -c $*.ml

%.cmx: %.ml %.cmi
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) $(LIBS) -c $*.ml

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLINC) $(LIBS) -c $*.mli

%.cmo: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(OCAMLINC) $(LIBS) -c $*.ml

%.cmx: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLOPTFLAGS) $(OCAMLINC) $(LIBS) -c $*.ml

%.ml: %.mll
	$(OCAMLLEX) $(OCAMLLEXFLAGS) $*.mll

%.ml %.mli: %.mly
	$(MENHIR) $(MENHIRFLAGS) $*.mly


# remove temporaries and binaries
#
clean:
	rm -f depend $(AUTOGEN) $(TARGET).byte $(TARGET).opt
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "\#*"`
	rm -f `find . -name "*.conflicts"`

.phony:	clean


# automatic dependencies
#
depend: $(MLFILES) $(MLIFILES)
	-$(OCAMLFIND) ocamldep -native $(OCAMLINC) $+ > depend

include depend
