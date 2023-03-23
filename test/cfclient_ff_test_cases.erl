% @doc
% Run the JSON tests in the git submodule ff-test-cases.
% @end
-module(cfclient_ff_test_cases).

-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

-define(TESTS_PATH, "test/ff-test-cases/tests").

% The original ff-test-cases are flaky (causing errors and forcing the SDK to
% return the default value), so only use new TestGrid test cases.
-define(
  NON_TEST_GRID_TESTS,
  [
    "bool_on_simple_rule.json",
    "off_flag.json",
    "off_off_no_rules.json",
    "on_off_no_rules.json",
    "prereq.json",
    "rules_priority.json",
    "segment_includes_target.json",
    "test_empty_or_missing_target_attributes.json"
  ]
).
-define(
  _assertEqualVars(Expect, Expr, Vars),
  ?_test(?assertEqual(Expect, Expr, (list_to_binary(io_lib:format("~p", Vars)))))
).
-define(_assertEqual(Expect, Expr, Comment), ?_test(?assertEqual(Expect, Expr, Comment))).

setup() ->
  % ?debugMsg("Running setup"),
  Modules = [cfclient_config],
  Config0 = [{name, ?MODULE}, {analytics_enabled, false}, {poll_enabled, false}],
  Config = cfclient_config:normalize(Config0),
  meck:new(Modules, [passthrough]),
  meck:expect(cfclient_config, get_config, fun () -> Config end),
  meck:expect(cfclient_config, get_config, fun (_) -> Config end),
  meck:expect(cfclient_config, defaults, fun () -> Config end),
  Modules.


cleanup(Modules) ->
  % ?debugFmt("Running cleanup ~p", [Modules]),
  meck:unload(Modules).


evaluations_test_() ->
  % Paths = lists:filter(fun (P) -> P == "test/ff-test-cases/tests/GroupRules/GroupRules_Type_Bool_State_Enabled_InOneTwoThree_Should_Return_true_for_Target_three.json" end,
  %           list_test_files(?TESTS_PATH)),
  Paths = list_test_files(?TESTS_PATH),
  {setup, fun setup/0, fun cleanup/1, lists:map(fun evaluate_file/1, Paths)}.


evaluate_file(Path) ->
  TestFile = parse_file(Path),
  #{flags := Flags, tests := Tests} = TestFile,
  Segments = maps:get(segments, TestFile, []),
  {
    Path,
    setup,
    fun
      () ->
        Config = [{name, ?MODULE}, {analytics_enabled, false}, {poll_enabled, false}, {unit_test_mode, true}],
        {ok, Pid} = cfclient_instance:start_link([{config, Config}]),
        [cfclient_cache:cache_flag(F) || F <- Flags],
        [cfclient_cache:cache_segment(S) || S <- Segments],
        Pid
    end,
    fun (Pid) -> gen_server:stop(Pid) end,
    [[evaluate_test(T, TestFile) || T <- Tests]]
  }.


% Config0 = [{name, ?MODULE}, {analytics_enabled, false}, {poll_enabled, false}],
% Config = cfclient_config:normalize(Config0),
% {ok, Pid} = cfclient_instance:start_link([{config, Config0}]),
% cfclient_config:set_config(Config),
% [cfclient_cache:cache_flag(F, Config) || F <- Flags],
% [cfclient_cache:cache_segment(S, Config) || S <- Segments],
% Results = {Path, [evaluate_test(T, TestFile) || T <- Tests]},
% gen_server:stop(Pid),
% Results.
evaluate_test(Test, TestFile) ->
  #{flag := Id, expected := Expected} = Test,
  #{flags := Flags} = TestFile,
  Target = find_target(Test, TestFile),
  #{kind := Kind} = find_flag(Id, Flags),
  case Kind of
    <<"boolean">> ->
      {Id, ?_assertEqual(Expected, cfclient:bool_variation(Id, Target, false), Target)};

    <<"string">> -> {Id, ?_assertEqual(Expected, cfclient:string_variation(Id, Target, "blue"))};
    <<"int">> -> {Id, ?_assertEqual(Expected, cfclient:number_variation(Id, Target, 100))};

    <<"json">> ->
      {
        Id,
        ?_assertEqual(Expected, jsx:encode(cfclient:json_variation(Id, Target, #{}), [{space, 1}]))
      }
  end.


-spec find_target(map(), map()) -> Target :: map().
find_target(#{target := Id}, #{targets := Targets}) when is_list(Targets) ->
  case lists:search(fun (#{identifier := I}) -> I == Id end, Targets) of
    {value, Value} -> Value;

    false ->
      ?debugFmt("Test target not found for test ~p ~p", [Id, Targets]),
      #{}
  end;

find_target(_Target, _TestFile) ->
  % ?debugFmt(" No target for test ~p ~p", [Target, TestFile]),
  #{}.


find_flag(Id, Flags) ->
  {value, Value} = lists:search(fun (#{feature := I}) -> I == Id end, Flags),
  Value.


-spec parse_file(file:filename()) -> map().
parse_file(Path) ->
  {ok, Data} = file:read_file(Path),
  % Same shape as Client API
  jsx:decode(Data, [return_maps, {labels, atom}]).


list_test_files(Dir) ->
  filelib:fold_files(
    Dir,
    ".json$",
    true,
    fun
      (F, Acc) ->
        case lists:member(filename:basename(F), ?NON_TEST_GRID_TESTS) of
          true -> Acc;
          false -> [F | Acc]
        end
    end,
    []
  ).
