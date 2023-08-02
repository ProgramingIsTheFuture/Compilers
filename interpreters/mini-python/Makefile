FLAGS=-annot -g

all: mini-python
	./mini-python test.py

.PHONY: tests
tests: mini-python
	bash run-tests

mini-python: $(CMO)
	dune build
	cp _build/default/main.exe ./mini-python

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

clean:
	rm -f *.o *.annot *~ mini-python
	rm -f parser.output parser.automaton

