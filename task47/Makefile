all: task7.native task4.native

task%: FORCE
	./preprocess.pl src/proofs.mlp >src/proofs.ml
	ocamlbuild -I src $@

FORCE:

clean:
	rm -rf _build
	rm -f *.byte *.native

.PHONY: clean all FORCE
