# Compile in -custom mode so there is no problem with finding the
# shared library dllmindstorm.so
PACKAGES = -package mindstorm,graphics
DOC_DIR = doc

OCAMLC_FLAGS = -g -dtypes -dllpath .. -custom $(PACKAGES) -I ..
OCAMLOPT_FLAGS = -dtypes $(PACKAGES) -I ..

INTERFACES=$(wildcard *.mli)
PROGRAM=run.ml
LIBS_CMA = -linkpkg robot.cmo
LIBS_CMXA = -linkpkg $(LIBS_CMA:.cma=.cmxa) robot.cmx

.PHONY: byte native
byte: $(PROGRAM:.ml=.exe)
native: $(PROGRAM:.ml=.com)

run.exe: labyrinth.cmo display.cmo solver.cmo


# Generate HTML documentation for these modules only
.PHONY: doc
doc: $(INTERFACES:.mli=.cmi)
	test -d $(DOC_DIR) || $(MKDIR) $(DOC_DIR)
	$(OCAMLDOC) -d $(DOC_DIR) -colorize-code -stars -html $(PACKAGES) \
	  $(INTERFACES)


# Define the OS type for RM,...
../.os_type: ../make_os_type.exe
	"$<" > $@
include ../.os_type

include ../Makefile.ocaml

clean::
	-$(RM) $(wildcard *.exe *.com)
	-$(RM) -rf $(DOC_DIR)