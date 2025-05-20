EXTRACTED_DIR = ../extraction
THEORIES_DIR = ../theories
MENHIR = menhir


.PHONY: all prepare

prepare_theories: $(THEORIES_DIR)/Cabs.v

prepare: $(EXTRACTED_DIR)/Cabs.v \
         $(EXTRACTED_DIR)/enter.ml \
         $(EXTRACTED_DIR)/lexer.ml \
				 $(EXTRACTED_DIR)/extraction.v

$(THEORIES_DIR)/Cabs.v: Cabs.v | $(THEORIES_DIR)
	cp $< $@

$(EXTRACTED_DIR):
	mkdir -p $@

$(EXTRACTED_DIR)/enter.ml: enter.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/lexer.ml: lexer.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/Cabs.v: Cabs.v | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/extraction.v: extraction.v | $(EXTRACTED_DIR)
	cp $< $@
