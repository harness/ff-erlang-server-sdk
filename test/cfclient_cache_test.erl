-module(cfclient_cache_test).

-include_lib("eunit/include/eunit.hrl").

get_from_cache_test() ->
  meck:new(cfclient_ets),
  %% Flag is found in cache
  meck:expect(cfclient_ets, get, fun (_, <<"flags/does_exist">>) -> <<"flags/does_exist">> end),
  ?assertEqual(
    {ok, <<"flags/does_exist">>},
    cfclient_cache:get_value({flag, <<"does_exist">>})
  ),
  %% Flag is not found in cache
  meck:expect(cfclient_ets, get, fun (_, <<"flags/does_not_exist">>) -> undefined end),
  ?assertEqual(
    {error, undefined},
    cfclient_cache:get_value({flag, <<"does_not_exist">>})
  ),
  %% Segment is found in cache
  meck:expect(
    cfclient_ets,
    get,
    fun (_, <<"segments/does_exist">>) -> <<"segments/does_exist">> end
  ),
  ?assertEqual(
    {ok, <<"segments/does_exist">>},
    cfclient_cache:get_value({segment, <<"does_exist">>})
  ),
  %% Segment is not found in cache
  meck:expect(cfclient_ets, get, fun (_, <<"segments/does_not_exist">>) -> undefined end),
  ?assertEqual(
    {error, undefined},
    cfclient_cache:get_value({segment, <<"does_not_exist">>})
  ),
  meck:unload(cfclient_ets).


format_key_test() ->
  %% Flag key
  ?assertEqual(<<"flags/flag_1">>, cfclient_cache:format_key({flag, <<"flag_1">>})),
  %% Segment key
  ?assertEqual(
    <<"segments/segment_1">>,
    cfclient_cache:format_key({segment, <<"segment_1">>})
  ).
