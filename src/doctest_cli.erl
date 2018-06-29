-module(doctest_cli).
-export([parse/1, get_files/1, get_code_paths/1]).

-record(parsed_args, { code_paths, sources }).

parse(Args) ->
    parse(Args, [], []).

parse([], CodePaths, Sources) ->
    #parsed_args{code_paths = CodePaths, sources = Sources};
parse(["-source", Source | Rest], CodePaths, Sources) ->
    parse(Rest, CodePaths, [Source | Sources]);
parse(["-pa", CodePath | Rest], CodePaths, Sources) ->
    parse(Rest, [CodePath | CodePaths], Sources);
parse([Other |_], _, _) ->
    exit({unknown_option, Other}).

get_files(ParsedArgs) ->
    ParsedArgs#parsed_args.sources.

get_code_paths(ParsedArgs) ->
    ParsedArgs#parsed_args.code_paths.
