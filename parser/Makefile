EXTRACTED_DIR = ../extraction
MENHIR = menhir

PARSER_SOURCE = Parser.vy
PARSER_OUTPUT = Parser.v
PRE_PARSER_SOURCE = Pre_parser.mly

.PHONY: all prepare

all: prepare

prepare: $(EXTRACTED_DIR)/$(PARSER_OUTPUT) \
         $(EXTRACTED_DIR)/Cabs.v \
         $(EXTRACTED_DIR)/extraction.v \
         $(EXTRACTED_DIR)/enter.ml \
         $(EXTRACTED_DIR)/lexer.ml \
         $(EXTRACTED_DIR)/pprint.ml \
         $(EXTRACTED_DIR)/Pre_parser.mly

$(EXTRACTED_DIR):
	mkdir -p $@

$(EXTRACTED_DIR)/$(PARSER_OUTPUT): $(PARSER_SOURCE) | $(EXTRACTED_DIR)
	$(MENHIR) --coq --coq-no-version-check --base Parser $< 
	mv Parser.v $@

$(EXTRACTED_DIR)/enter.ml: enter.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/lexer.ml: lexer.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/pprint.ml: pprint.ml | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/Cabs.v: Cabs.v | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/extraction.v: extraction.v | $(EXTRACTED_DIR)
	cp $< $@

$(EXTRACTED_DIR)/Pre_parser.mly: $(PRE_PARSER_SOURCE) | $(EXTRACTED_DIR)
	cp $< $@
