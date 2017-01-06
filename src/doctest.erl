%% TODO define wether its ok to use functions, or if it is to be run as a single
%% function, where each statement must either be delimited by ',', or by '.' if
%% it is the last statement.
%%
%% Or: State that runnable chunks are to be used like one would use an
%% interpreter session.
%%
%% TODO NOTE: no epp - no preprocessing
%% TODO mention that ``` must be matched exactly => ```no_run works OOTB
-module(doctest).
-export([main/1]).

%% Run tests for all files
main(Args) when is_list(Args) ->
    ParsedArgs = doctest_cli:parse(Args),
    CodePaths = doctest_cli:get_code_paths(ParsedArgs),
    ok = add_code_paths(CodePaths),
    case doctest_cli:get_files(ParsedArgs) of
        [] -> io:format("No tests to run.~n");
        Files -> [ run(File) || File <- Files ]
    end.

add_code_paths(Paths) ->
    lists:foreach(fun code:add_patha/1, Paths).

run(File) when is_binary(File);
                is_list(File) ->
    %% Create tests
    Comments = erl_comment_scan:file(File),
    {ok,RawChunks} = scan(Comments),
    CreatedModules = compile_and_load(RawChunks),
    %% TODO output which test is currently running
    [ M:run() || M <- CreatedModules ].

%% Scan for comments which indicate a runnable block.
scan(Comments) -> % -> [Chunk of code]
    %% TODO it might be a good idea to assert that a block is contained in a
    %% consecutive comment -> Then we could make use of line numbering as well
    %% (need line numbers for error display)
    %% TODO if the above is done, drop lists:append
    scan(lists:append(drop_position_indicator(Comments)), not_in_block, []).

scan([], not_in_block, Acc) ->
    {ok,Acc};
scan([], _, _Acc) ->
    exit(last_block_not_completed);
scan([Comment|T], not_in_block, Acc) ->
    %% remove leading comment character
    Sanitized = sanitize(Comment),
    case is_backticks(Sanitized) of
        true -> scan(T, [], Acc);
        false -> scan(T, not_in_block, Acc)
    end;
scan([Comment|T], CurrentRun, Acc) when is_list(CurrentRun) ->
    Sanitized = sanitize(Comment),
    case is_backticks(Sanitized) of
        false -> scan(T, [Sanitized|CurrentRun], Acc);
        true -> scan(T, not_in_block, [lists:reverse(CurrentRun)|Acc])
    end.

is_backticks(String) ->
    String =:= "```" orelse String =:= <<"```">>.

sanitize(Comment) -> %% -> binary()
    [_, DroppedComment] = re:split(Comment, "%+ *", [{parts,2}, {return,binary}]),
    DroppedComment.

drop_position_indicator(Comments) ->
    %% See erl_comment_scan:file/1 for the type of Comments
    [ InnerComments || {_,_,_,InnerComments} <- Comments ].

compile_and_load(Chunks) -> % -> [Modules]
    [ compile_and_load1(Chunk) || Chunk <- Chunks ].

compile_and_load1(Chunk) ->
    %% TODO incorporate info on chunk (file, line) in created module - pad
    %% generated function to the line where original chunk is -> improves error
    %% messages
    %% TODO check if chunk ends with '.'; if not, add it
    Chunk1 = [ binary_to_list(Bin) || Bin <- Chunk ],
    ModuleName = lists:flatten(
                   io_lib:format(
                     "doctest_~B", [erlang:unique_integer([positive])])),
    Abstract = ["-module(" ++ ModuleName ++ ").",
                "-export([run/0]).",
                "run() ->"
                |
                Chunk1
               ],
    Joined = string:join(Abstract, "\n"),
    {ok,Scanned,_} = erl_scan:string(Joined),
    {[],SplitTokensRev} =
        lists:foldl(
          fun (D={dot,_}, {Cur,Done}) ->
                  {[], [lists:reverse([D|Cur])|Done]};
              (E, {Cur,Done}) ->
                  {[E|Cur],Done}
          end, {[],[]}, Scanned),
    SplitTokens = lists:reverse(SplitTokensRev),
    Forms = [ begin
                  {ok,F} = erl_parse:parse_form(T),
                  F
              end || T <- SplitTokens ],
    BinModuleName = list_to_atom(ModuleName),
    {ok,BinModuleName,Bin} = compile:forms(Forms),
    {module,_} = code:load_binary(BinModuleName, "nofile", Bin),
    BinModuleName.
