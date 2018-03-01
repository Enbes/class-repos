OCAMLBUILD=ocamlbuild
OCAMLBUILD_OPTS=-use-menhir -use-ocamlfind
TEST_DIR=tests
TEST_SCRIPT=run.sh
DOC_DIR=grumpy.docdir

TARGET=grumpy

.PHONY: all test clean

all: 
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(TARGET).native

js:
	ocamlbuild -use-ocamlfind -pkgs "js_of_ocaml,js_of_ocaml.syntax" \
	-use-menhir -syntax camlp4o grumpyjs.byte
	js_of_ocaml +nat.js +weak.js grumpyjs.byte

doc:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(DOC_DIR)/index.html

test: all
	cd $(TEST_DIR) && ./$(TEST_SCRIPT)

clean:
	rm -f $(TARGET).native *~ \#*
	$(OCAMLBUILD) -clean
