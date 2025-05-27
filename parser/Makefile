EXTRACTED_DIR = ../extraction
MENHIR = menhir


.PHONY: all prepare

prepare: $(EXTRACTED_DIR)/Cabs.v \
         $(EXTRACTED_DIR)/enter.ml \
         $(EXTRACTED_DIR)/Lexer.v \
				 $(EXTRACTED_DIR)/extraction.v \
				 $(EXTRACTED_DIR)/Schema.v \
				 $(EXTRACTED_DIR)/regex.ml


$(EXTRACTED_DIR):
	mkdir -p $@

$(EXTRACTED_DIR)/enter.ml: enter.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/regex.ml: regex.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/Lexer.v: Lexer.v | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/Cabs.v: Cabs.v | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/extraction.v: extraction.v | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/Schema.v: ../schema/Schema.v | $(EXTRACTED_DIR)
	cp $< $@

