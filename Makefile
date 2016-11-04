all: ebin/doctest.beam ebin/doctest.escript ebin/ex1.beam ebin/failing_edgecase.beam ebin/failing.beam

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

ebin/failing_edgecase.beam: examples/failing_edgecase.erl
	erlc -o ebin $<

ebin/failing.beam: examples/failing.erl
	erlc -o ebin $<
