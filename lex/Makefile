
all:
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -o obj lexer.cmo parser.cmo

clean:
	rm ./*.cmo
	rm ./*.cmi
	rm ./lexer.ml
	rm ./obj

restart:
	make clean && make all

.PHONY: all clean restart
