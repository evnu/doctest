-module(failing_edgecase).

-export([something/0]).

something() ->
    %% ```
    %% % Code in functions is checked as well for now.
    %% io:format("No problems with side effects~n").
    %% ```
    ok.
