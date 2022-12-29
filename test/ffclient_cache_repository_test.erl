-module(ffclient_cache_repository_test).

-include_lib("eunit/include/eunit.hrl").

get_from_cache_test() ->
  PID = self(),

  %% Mock LRU Cache
  meck:new(lru),

  %% Flag is found in cache
  meck:expect(lru, get,  fun(PID, <<"flags/does_exist">>) -> <<"flags/does_exist">> end),
  ?assertEqual(<<"flags/does_exist">>, ffclient_cache_repository:get_from_cache({flag, <<"does_exist">>}, PID)),

  %% Flag is not found in cache
  meck:expect(lru, get,  fun(PID, <<"flags/does_not_exist">>) -> undefined end),
  ?assertEqual(undefined, ffclient_cache_repository:get_from_cache({flag, <<"does_not_exist">>}, PID)),

  %% Segment is found in cache
  meck:expect(lru, get,  fun(PID, <<"segments/does_exist">>) -> <<"segments/does_exist">> end),
  ?assertEqual(<<"segments/does_exist">>, ffclient_cache_repository:get_from_cache({segment, <<"does_exist">>}, PID)),

  %% Segment is not found in cache
  meck:expect(lru, get,  fun(PID, <<"segments/does_not_exist">>) -> undefined end),
  ?assertEqual(undefined, ffclient_cache_repository:get_from_cache({segment, <<"does_not_exist">>}, PID)),

  meck:unload(lru).

format_key_test() ->
  %% Flag key
  ?assertEqual(<<"flags/flag_1">>, ffclient_cache_repository:format_key({flag, <<"flag_1">>})),

  %% Segment key
  ?assertEqual(<<"segments/segment_1">>, ffclient_cache_repository:format_key({segment, <<"segment_1">>})).