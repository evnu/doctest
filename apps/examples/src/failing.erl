-module(failing).
-export([a/0]).

%% This is supposed to fail
%% ```
%% fail = failing:a().
%% ```
a() -> no_fail.
