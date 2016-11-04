#!/usr/bin/escript

main([_Ebin]) ->
    usage();
main([Ebin|Files]) ->
    code:add_patha(Ebin),
    doctest:run(Files).

usage() ->
    io:format("doctest <Files..>~n").
