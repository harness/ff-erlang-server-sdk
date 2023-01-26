-module(cfclient_cache_tests).

-include_lib("eunit/include/eunit.hrl").

setup() ->
  Modules = [cfclient_config, cfclient_ets],
  Config = cfclient_config:defaults(),
  meck:new(Modules, [passthrough]),
  meck:expect(cfclient_config, get_config, fun () -> Config end),
  meck:expect(cfclient_config, get_config, fun (_) -> Config end),
  meck:expect(cfclient_config, defaults, fun () -> Config end),
  Modules.


cleanup(Modules) ->
  meck:unload(Modules).

top_test_() ->
  {
    setup,
    fun setup/0,
    fun cleanup/1,
    [
      {generator, fun get_from_cache/0}
    ]
  }.

config() -> cfclient_config:get_config().

-define(_expectGet(Key, Value),
        (fun () -> (meck:expect(cfclient_ets, get, fun (_, (Key)) -> (Value) end)) end)).

get_from_cache() ->
  [
   {"Flag is found in cache", setup,
    ?_expectGet(<<"flags/does_exist">>, <<"flags/does_exist">>),
    ?_assertEqual(
       {ok, <<"flags/does_exist">>},
       cfclient_cache:get_value({flag, <<"does_exist">>}, config())
      )
   },
   {"Flag is not found in cache", setup,
    ?_expectGet(<<"flags/does_not_exist">>, undefined),
    ?_assertEqual({error, undefined}, cfclient_cache:get_value({flag, <<"does_not_exist">>}, config()))
   },
   {"Segment is found in cache",
    setup,
    ?_expectGet(<<"segments/does_exist">>, <<"segments/does_exist">>),
    ?_assertEqual(
      {ok, <<"segments/does_exist">>},
      cfclient_cache:get_value({segment, <<"does_exist">>}, config())
    )
   },
   {"Segment is not found in cache",
    setup,
    ?_expectGet(<<"segments/does_not_exist">>, undefined),
    ?_assertEqual(
      {error, undefined},
      cfclient_cache:get_value({segment, <<"does_not_exist">>}, config())
    )
   }
  ].

format_key_test_() ->
  [
   {"Flag key", ?_assertEqual(<<"flags/flag_1">>, cfclient_cache:format_key({flag, <<"flag_1">>}))},
   {"Segment key", ?_assertEqual(<<"segments/segment_1">>, cfclient_cache:format_key({segment, <<"segment_1">>}))}
  ].
