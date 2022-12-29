%%%-------------------------------------------------------------------
%%% @doc
%%% %% Runs the JSON tests in the git submodule ff-test-cases
%%% @end
%%%-------------------------------------------------------------------
-module(ffclient_ff_test_cases).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

-define(TESTS_PATH, "test/ff-test-cases/tests").
%% The original ff-test-cases are flaky (causing errors and forcing the SDK to return the default value), so only use new TestGrid test cases.
-define(NON_TEST_GRID_TESTS, ["bool_on_simple_rule.json", "off_flag.json", "off_off_no_rules.json",
  "on_off_no_rules.json", "prereq.json", "rules_priority.json",
  "segment_includes_target.json", "test_empty_or_missing_target_attributes.json"]).


evaluations_test_() ->
  {ok, TestFiles} = load_test_files(?TESTS_PATH),
  %% Disable analytics as we call the public variation functions.
  ffclient_config:init("fake_key", #{analytics_enabled => false}),
  evaluate_test_files(TestFiles, []).

evaluate_test_files([Head | Tail], Accu) ->
  %% Parse each file into a map - e.g. we can get The Flags, Targets, Tests
  TestAsMap = test_file_json_to_map(Head),
  %% Create new LRU cache and load Flags and Groups into it
  {ok, CachePID} = start_lru_cache(),
  ffclient_cache_repository:set_pid(CachePID),
  cache_flags_and_groups(CachePID, maps:get(flags, TestAsMap), maps:get(segments, TestAsMap, [])),
  Result = evaluate_tests(maps:get(tests, TestAsMap), maps:get(targets, TestAsMap), CachePID, Accu),
  lru:stop(CachePID),
  evaluate_test_files(Tail,  Result);
evaluate_test_files([], Accu) -> Accu.

evaluate_tests([Head | Tail], Targets, CachePID, Accu) ->
  %% Get correct Target for test case
  Target =
    case maps:is_key(target, Head) of
      true ->
        GetTarget =
          fun
            F([H | T]) ->
              case string:equal(maps:get(identifier, H), maps:get(target, Head), false) of
                true ->
                  H;
                false ->
                  F(T)
              end;
            F([]) -> logger:error("Target for test case not found")
          end, GetTarget(Targets);
      %% If no Target for test case then just return an empty map
      false ->
        #{}
    end,

  Flag = ffclient_cache_repository:get_from_cache({flag, maps:get(flag, Head)}, CachePID),
  Kind = maps:get(kind, Flag),

  FlagIdentifier = maps:get(flag, Head),
  Result = case Kind of
    <<"boolean">> ->
      ffclient:bool_variation(maps:get(flag, Head), Target, false);
    <<"string">> ->
      list_to_binary(ffclient:string_variation(FlagIdentifier, Target, "blue"));
    <<"int">> ->
      ffclient:number_variation(FlagIdentifier, Target, 100);
    <<"json">> ->
      jsx:encode(ffclient:json_variation(FlagIdentifier, Target, #{}), [{space, 1}])
  end,
  Test = {FlagIdentifier, ?_assertEqual(maps:get(expected, Head), Result)},
  evaluate_tests(Tail, Targets, CachePID, [Test | Accu]);
evaluate_tests([], _, _, Accu) -> Accu.

cache_flags_and_groups(CachePID, Flags, Groups) ->
  [ffclient_cache_repository:set_to_cache({flag, maps:get(feature, Flag)}, Flag, CachePID) || Flag <- Flags],
  [ffclient_cache_repository:set_to_cache({segment, maps:get(identifier, Segment)}, Segment, CachePID) || Segment <- Groups].

start_lru_cache() ->
  Size = 32000000,
  CacheName = ffclient_cache_default,
  lru:start_link({local, CacheName},[{max_size, Size}]).

test_file_json_to_map(Json) ->
  {ok, Data} = file:read_file(Json),
  %% Same shape as Client API
  jsx:decode(Data, [return_maps, {labels, atom}]).


load_test_files(Dir) ->
  load_test_files(Dir, true).

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
        SubPaths = [filename:join(Path, Name) || Name <- Listing, lists:member(Name, ?NON_TEST_GRID_TESTS) == false],
        walk_directory(SubPaths, FilesOnly,
          case FilesOnly of
            true -> Acc;
            false -> [Path | Acc]
          end)
    end).
