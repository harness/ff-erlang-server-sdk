%%%-------------------------------------------------------------------
%%% @doc
%%% %% Runs the JSON tests in the git submodule ff-test-cases
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_ff_test_cases).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

-define(TESTS_PATH, "test/ff-test-cases/tests").


evaluations_test() ->
  %% Loop through list returned by load_test files
  %% Load
  TestFiles = load_test_files(?TESTS_PATH),
  evaluate_test_files(TestFiles),
  ?assert(true),
  asd.

evaluate_test_files([Head | Tail]) ->
  %% Parse each file into a map - e.g. we can get The Flags, Targets, Tests
  TestAsMap = test_file_json_to_map(Head),
  %% Create new LRU cache and load Flags and Groups into it
  {ok, CachePID} = start_lru_cache(),
  cache_flags_and_groups(CachePID, maps:get(flags, TestAsMap), maps:get(segments, TestAsMap, [])),
  evaluate_tests(maps:get(tests, TestAsMap), maps:get(targets, TestAsMap)),

  asd.

evaluate_tests([Head | Tail], Targets) ->
  %% Get correct Target for test case
  GetTarget =
    fun
      F([H | T]) ->
        Target = maps:get(target, Head),
        case string:equal(maps:get(target, Head), Target, false) of
          true ->
            Target;
          false ->
            F(T)
        end;
      F([]) -> logger:error("Target for test case not found")
    end,
  GetTarget(Targets),
  asd.

cache_flags_and_groups(CachePID, Flags, Groups) ->
  [cfclient_cache_repository:set_to_cache({flag, maps:get(feature, Flag)}, Flag, CachePID) || Flag <- Flags],
  [cfclient_cache_repository:set_to_cache({segment, maps:get(identifier, Segment)}, Segment, CachePID) || Segment <- Groups].

start_lru_cache() ->
  Size = 32000000,
  CacheName = cfclient_cache_default,
  lru:start_link({local, CacheName},[{max_size, Size}]).

test_file_json_to_map(Json) ->
  {ok, Data} = file:read_file(?TESTS_PATH),
  %% Same shape as Client API
  jsx:decode(Data, [return_maps, {labels, atom}]).


load_test_files(Dir) ->
  load_test_files(Dir, false).

load_test_files(Dir, FilesOnly) ->
  case filelib:is_file(Dir) of
    true ->
      case filelib:is_dir(Dir) of
        true -> {ok, walk_directory([Dir], FilesOnly, [])};
        false -> {error, enotdir}
      end;
    false -> {error, enoent}
  end.

walk_directory([], _FilesOnly, Acc) -> Acc;
walk_directory([Path|Paths], FilesOnly, Acc) ->
  walk_directory(Paths, FilesOnly,
    case filelib:is_dir(Path) of
      false -> [Path | Acc];
      true ->
        {ok, Listing} = file:list_dir(Path),
        SubPaths = [filename:join(Path, Name) || Name <- Listing, filename:extension(Name) == ".json"],
        walk_directory(SubPaths, FilesOnly,
          case FilesOnly of
            true -> Acc;
            false -> [Path | Acc]
          end)
    end).
