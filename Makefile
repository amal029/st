CC=ocamlopt -annot -g
PARSERLIB=parser.cmxa
LANGUAGELIB=systemj.cmxa
ERRORLIB=error.cmxa

all: compile


compile:
	$(MAKE) -e -C error/ all
	$(MAKE) -e -C language/ all
	$(MAKE) -e -C parser/ all
	ocamlfind $(CC) -pp "camlp4o pa_macro.cmo -UDEBUG -USDEBUG" -o	\
	systemjc -syntax batteries.syntax -linkpkg -package batteries	\
	-package sexplib -I ./language -I ./error -I ./parser		\
	$(ERRORLIB) $(LANGUAGELIB) $(PARSERLIB) stc.ml

clean:
	$(MAKE) -e -C language/ clean
	$(MAKE) -e -C error/ clean
	$(MAKE) -e -C parser/ clean
	rm -rf *.ll *.lle *.bc *.s *.dot *.grf *.part* gmon.out TAGS *.mli *.cm* *.o systemjc \
	*.xml *.annot *_spi* *_ver* *.pml.trail 
