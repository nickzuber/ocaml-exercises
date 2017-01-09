
# Build OCaml projects with ocamlbuild

OCB_FLAGS = -use-ocamlfind -pkg core -tags thread 
OCB = 		ocamlbuild $(OCB_FLAGS)

# Unless we can figure out a way to compile multiple files together
# or change extensions of files to %.ml -> %.native then we have
# to compile one file at a time. I probably just don't understand
# enough about compilation yet
OCAML_SRC = main/main.ml\
						basic/basic.ml\
						typec/typec.ml

all: native

run: 
	make native && ./typec.native

native: $(OCAML_SRC)
	$(OCB) -Is typec/cmds typec/typec.native

clean:
	$(OCB) -clean

.PHONY:
	clean

