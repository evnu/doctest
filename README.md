# doctest

This is a proof-of-concept for Erlang, introducing automated testing of source code contained within
comments (module- or function-level). The idea is taken from [Rust's](https://www.rust-lang.org)
approach of embedding examples in comments and checking them as an extra run during testing.

## General Idea

The general idea is simple: Scan a given file for all comments, search for blocks enclosed in
backticks, wrap each such block in a function within a module definition and compile that definition
using merl. Then run the newly created function.

## Format of runnable code

To declare some part of the code as runnable, simply enclose it in backtics:

    %% This is a normal comment
    %% ```
    %% % This will be runnable
    %% hello = hello.
    %% ```

Note that you must end the chunk with a `.`, as this will close the generated function. Hence,
this would be legal as well (but should probably be avoided):

    %% ```
    %% another_function().
    %%
    %% another_function() ->
    %%      ok.
    %% ```

Here, the generated function ends after calling `another_function()`. As this is a proof-of-concept,
edge cases like this are considered undefined behaviour for now.

## Running Examples
The PoC contains a small set of examples in `apps/examples`. They are build automatically by
erlang.mk when invoking `make`. You can run an example from the command line:

    erl -pa ebin -pa apps/examples/ebin -eval 'doctest:run(["apps/examples/src/ex1.erl"]), init:stop().'

This is not very convenient yet, but it works.

## Some Requirements

* When calling functions within the module which contains the runnable doc, ensure that
  the module is available in the code path.

## Some TODOs

- [ ] Provide a nicer way to run tests
