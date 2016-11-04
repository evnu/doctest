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
-export([run/1]).

%% Run tests for all files
run(Files) when is_list(Files) ->
    [ run1(File) || File <- Files ].

run1(File) when is_binary(File);
                is_list(File) ->
    %% TODO compile and load File to make the module available when running tests
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
    ModuleName = list_to_binary(
                   io_lib:format(
                     "doctest_~B", [erlang:unique_integer([positive])])),
    Abstract = [<<"-module(", ModuleName/binary, ").">>,
                <<"-export([run/0]).">>,
                <<"run() ->">>
                | Chunk
               ],
    Quoted = merl:quote(Abstract),
    {ok,_} = merl:compile_and_load(Quoted),
    binary_to_atom(ModuleName, utf8).
