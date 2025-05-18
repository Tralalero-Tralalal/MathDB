all:
	$(MAKE) -C parser
	
clean:
	find . -type f \( \
	    -name "*.vo" -o -name "*.glob" -o -name "*.vok" -o -name "*.vos" -o \
	    -name ".*.aux" -o -name ".depend" -o -name "*.byte" -o -name "*.native" -o \
	    -name "*.o" -o -name "*.cm*" -o -name "*.d.byte" -o -name "*.d.native" -o \
	    -name "*.ml.d" -o -name "Parser.v" -o -name "Parser.mli" -o -name "Parser.ml" \
	\) -exec rm -f {} +
