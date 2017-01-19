
# Build OCaml projects with ocamlbuild

OCB_FLAGS = -use-ocamlfind -pkg core -tags thread 
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native

run: 
	make native && ./parser_test.native

native:
	$(OCB) lex/parser_test.native

clean:
	$(OCB) -clean

.PHONY:
	clean

