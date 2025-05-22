# === Paths ===
EXTRACTED_DIR = extraction
THEORIES_DIR = theories
PARSER_DIR = parser
SCHEMA_DIR = schema

# === Tools ===
COQC = coqc -I . -Q $(EXTRACTED_DIR) Proj  
COQDEP = coqdep -Q $(EXTRACTED_DIR) Proj 
OCAMLBUILD = ocamlbuild -use-ocamlfind -pkg zarith -pkg str 

# === V Files and Targets ===
VFILES_EXE = $(EXTRACTED_DIR)/Cabs.v $(EXTRACTED_DIR)/Lexer.v $(EXTRACTED_DIR)/Pages.v $(EXTRACTED_DIR)/extraction.v  
VOFILES_EXE = $(VFILES_EXE:.v=.vo)

VFILES_THEORIES = $(THEORIES_DIR)/Cabs.v $(THEORIES_DIR)/Pages.v $(THEORIES_DIR)/Lexer.v  
VOFILES_THEORIES = $(VFILES_THEORIES:.v=.vo)

.PHONY: all build_exe build_theories prepare prepare_theories ocaml-build clean depend

all: build_exe

build_exe: prepare $(VOFILES_EXE) ocaml-build

prepare:
	$(MAKE) -C $(PARSER_DIR) prepare

theories: prepare_theories $(VOFILES_THEORIES)

prepare_theories: | $(THEORIES_DIR)
	cp $(PARSER_DIR)/Cabs.v $(THEORIES_DIR)/
	cp $(SCHEMA_DIR)/Pages.v $(THEORIES_DIR)/
	cp $(PARSER_DIR)/Lexer.v $(THEORIES_DIR)/

$(EXTRACTED_DIR)/%.vo: $(EXTRACTED_DIR)/%.v
	$(COQC) $<

$(THEORIES_DIR)/%.vo: $(THEORIES_DIR)/%.v
	coqc -Q $(THEORIES_DIR) Proj $<

ocaml-build:
	cd $(EXTRACTED_DIR) && $(OCAMLBUILD) enter.native && mv enter.native ../

depend:
	$(COQDEP) $(VFILES_EXE) > .depend

clean:
	find . -type f \( \
	    -name "*.vo" -o -name "*.glob" -o -name "*.vok" -o -name "*.vos" -o \
	    -name ".*.aux" -o -name ".depend" -o -name "*.byte" -o -name "*.native" -o \
	    -name "*.o" -o -name "*.cm*" -o -name "*.d.byte" -o -name "*.d.native" -o \
	    -name "*.ml.d" -o -name "Parser.v" -o -name "Parser.mli" -o -name "Parser.ml" \
	\) -exec rm -f {} +
	find $(EXTRACTED_DIR)/ -type f \
	    \( ! -name "_tags" -a ! -name "_CoqProject" -a ! -path "$(EXTRACTED_DIR)/MenhirLib/*" \) \
	    -exec rm -f {} +
	find $(THEORIES_DIR)/ -type f \
	    \( ! -name "_CoqProject" \) \
	    -exec rm -f {} +

-include .depend
