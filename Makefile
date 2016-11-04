all: ebin/doctest.beam ebin/doctest.escript ebin/ex1.beam

ebin/:
	mkdir ebin/

ebin/doctest.escript: src/doctest.escript
	cp src/doctest.escript ebin

ebin/doctest.beam: ebin src/doctest.erl
	erlc -o ebin src/doctest.erl

clean:
	rm -r ebin/

ebin/ex1.beam: examples/ex1.erl
	erlc -o ebin $<
