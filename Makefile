
# Build OCaml projects with ocamlbuild

OCB_FLAGS = -use-ocamlfind -pkg core,ppx_deriving.show -tags thread 
OCB = 		ocamlbuild $(OCB_FLAGS)

all: native

run: 
	make native && ./main.native

native:
	$(OCB) main/main.native

clean:
	$(OCB) -clean

.PHONY:
	clean

