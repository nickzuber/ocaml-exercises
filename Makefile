
# Build OCaml projects with ocamlbuild

OCB_FLAGS = -use-ocamlfind -pkg core -tags thread 
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native

run: 
	make native && ./typec.native

native:
	$(OCB) -I typec/cmds typec/typec.native

clean:
	$(OCB) -clean

.PHONY:
	clean

