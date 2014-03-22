OCMLOPTS=-a
OCMXALDEBUG=-g
CC=ocamlopt -g
YACC=ocamlyacc
LEX=ocamllex
LANGDIR=../language


all: parser

parser:
	$(YACC) -v parser.mly
	$(LEX) lexer.mll
	$(CC) -I $(LANGDIR) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -I $(LANGDIR) -c parser.ml
	$(CC) -a lexer.cmx parser.cmx -o parser.cmxa

clean:
	rm -rf *o *cm* parser.mli lexer.mli *output lexer.ml parser.ml language.mli error.mli *.a
