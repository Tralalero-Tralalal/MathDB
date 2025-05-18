COQC = coqc -Q . Proj -Q MenhirLib MenhirLib
COQDEP = coqdep -Q . Proj -Q MenhirLib MenhirLib
MENHIR = menhir
OCAMLBUILD = ocamlbuild -use-ocamlfind -use-menhir

# Menhir source and output
PARSER_SOURCE = Parser.vy
PARSER_OUTPUT = Parser.v

PRE_PARSER_SOURCE = Pre_parser.mly

# Paths
EXTRACTED_DIR = ../extraction

# Coq files
VFILES = Cabs.v $(PARSER_OUTPUT) extraction.v
VOFILES := $(VFILES:.v=.vo)

# Default target
all: $(VOFILES) ocaml-build clean

# Generate Parser.v from Parser.vy
$(PARSER_OUTPUT): $(PARSER_SOURCE)
	$(MENHIR) --coq --coq-no-version-check $<

# Compile Coq files
%.vo: %.v
	$(COQC) $<

# Rule: make sure extraction/ exists
$(EXTRACTED_DIR):
	mkdir -p $@

# Copy enter.ml into extraction/
$(EXTRACTED_DIR)/enter.ml: enter.ml | $(EXTRACTED_DIR)
	cp $< $@

# Copy lexer.ml into extraction/
$(EXTRACTED_DIR)/lexer.ml: lexer.ml | $(EXTRACTED_DIR)
	cp $< $@

# Copy pprint.ml into extraction/
$(EXTRACTED_DIR)/pprint.ml: pprint.ml | $(EXTRACTED_DIR)
	cp $< $@

# Copy pre_parser.mly into extraction/
$(EXTRACTED_DIR)/Pre_parser.mly: Pre_parser.mly | $(EXTRACTED_DIR)
	cp $< $@

# Use ocamlbuild to build the OCaml program
ocaml-build: $(EXTRACTED_DIR)/enter.ml $(EXTRACTED_DIR)/lexer.ml $(EXTRACTED_DIR)/pprint.ml $(EXTRACTED_DIR)/Pre_parser.mly
	cd $(EXTRACTED_DIR) && \
	$(OCAMLBUILD) -use-ocamlfind -use-menhir enter.native

# Dependency generation
depend:
	$(COQDEP) $(VFILES) > .depend

# Clean
clean:
	rm -f *.vo *.glob *.vok *.vos .*.aux .depend \
	      Parser.v Parser.mli Parser.ml enter.byte \
	      *.byte *.o *.cm* *.d.byte *.d.native *.ml.d
	find $(EXTRACTED_DIR) -type f ! -name "_tags" -exec rm -f {} +

dump-conflicts:
	$(MENHIR) --explain --dump $(PRE_PARSER_SOURCE)
-include .depend
