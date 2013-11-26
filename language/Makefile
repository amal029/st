OCMLOPTS=-a
OCMALDEBUG=-g
CC=ocamlopt -annot -g
SRC=st.ml
TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
BATDIR=`ocamlfind query batteries`

all: systemj.cmxa

systemj.cmxa: check
	$(CC) $(OCMLOPTS) st.cmx -o $@

check:
	@echo $(TYPECONV)
	$(CC) -pp "camlp4o -I $(TYPECONV) -I $(SEXPLIB)			\
	pa_type_conv.cma pa_sexp_conv.cma pa_macro.cmo -UDEBUG" -I	\
	$(BATDIR) -I $(SEXPLIB) -I ../error -c $(SRC)
clean:
	rm -rf *cm* *o *a *mli *.annot
