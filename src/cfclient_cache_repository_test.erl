-module(cfclient_cache_repository_test).
-author("erowlands").

-include_lib("eunit/include/eunit.hrl").

 slow_test_() ->
           {timeout, 60,
          fun() ->
                  timer:sleep(10000)
           end}.

get_from_cache_test() ->
  PID = self(),

  %% Flag is found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "flags/does_exist") -> "flags/does_exist" end),
  ?assertEqual("flags/does_exist", cfclient_cache_repository:get_from_cache({flag, "does_exist"}, PID)),
  meck:unload(lru),

  %% Flag is not found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "flags/does_not_exist") -> undefined end),
  ?assertEqual(undefined, cfclient_cache_repository:get_from_cache({flag, "does_not_exist"}, PID)),
  meck:unload(lru),

  %% Segment is found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "segments/does_exist") -> "segments/does_exist" end),
  ?assertEqual("segments/does_exist", cfclient_cache_repository:get_from_cache({segment, "does_exist"}, PID)),
  meck:unload(lru),

  %% Segment is not found in cache
  meck:new(lru),
  meck:expect(lru, get,  fun(PID, "segments/does_not_exist") -> undefined end),
  ?assertEqual(undefined, cfclient_cache_repository:get_from_cache({segment, "does_not_exist"}, PID)),
  meck:unload(lru).

format_key_test() ->
  {timeout,3600},
  %% Flag key
  ?assertEqual("flags/flag_1", cfclient_cache_repository:format_key({flag, "flag_1"})),

  %% Segment key
  ?assertEqual("segments/segment_1", cfclient_cache_repository:format_key({segment, "segment_1"})).
